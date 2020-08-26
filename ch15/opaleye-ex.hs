{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import Opaleye
import Data.Profunctor
import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.FromField as PGS hiding (Field)

import Data.Int
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import System.Random


data Rating = G | PG | PG13 | R | NC17
  deriving Show

data PGRating

fromRating :: IsString p => Rating -> p
fromRating G = "G"
fromRating PG = "PG"
fromRating PG13 = "PG-13"
fromRating R = "R"
fromRating NC17 = "NC-17"

toMaybeRating :: (Eq p, IsString p) => p -> Maybe Rating
toMaybeRating "G" = Just G
toMaybeRating "PG" = Just PG
toMaybeRating "PG-13" = Just PG13
toMaybeRating "R" = Just R
toMaybeRating "NC-17" = Just NC17
toMaybeRating _ = Nothing

instance PGS.FromField Rating where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) =
    case toMaybeRating bs of
      Nothing -> returnError ConversionFailed f (B.unpack bs)
      Just r -> pure r

instance DefaultFromField PGRating Rating where
  defaultFromField = fieldQueryRunnerColumn

instance pgf ~ Field PGRating => Default ToFields Rating pgf where
  def = dimap fromRating (unsafeCast "mpaa_rating")
              (def :: ToFields Text (Field PGText))

data FilmInfo' i t si r = FilmInfo {
    filmId :: i
  , title :: t
  , description :: t
  , releaseYear :: si
  , filmLength :: si
  , rating :: r
  }

type FilmInfo = FilmInfo' Int64 Text Int32 Rating

deriving instance Show FilmInfo

type FilmInfoField =
  FilmInfo' (Field SqlInt8) (Field SqlText)
            (Field SqlInt4) (Field PGRating)

type FilmInfoFieldWrite =
  FilmInfo' (Maybe (Field SqlInt8)) (Field SqlText)
            (Field SqlInt4) (Field PGRating)

makeAdaptorAndInstance "pFilmInfo" ''FilmInfo'

filmTable :: Table FilmInfoFieldWrite FilmInfoField
filmTable =
  table "film"
        (pFilmInfo FilmInfo {
              filmId = tableField "film_id"
            , title = tableField "title"
            , description = tableField "description"
            , releaseYear = tableField "release_year"
            , filmLength = tableField "length"
            , rating = tableField "rating"
            })

categoryTable :: Table (Maybe (Field SqlInt8), Field SqlText)
                       (Field SqlInt8, Field SqlText)
categoryTable =
  table "category"
        (p2 ( tableField "category_id"
            , tableField "name"))

filmCategoryTable :: Table (Field SqlInt8, Field SqlInt8)
                           (Field SqlInt8, Field SqlInt8)
filmCategoryTable =
  table "film_category"
        (p2 ( tableField "film_id"
            , tableField "category_id"))

filmSelect :: Select FilmInfoField
filmSelect = selectTable filmTable

categorySelect :: Select (Field SqlInt8, Field SqlText)
categorySelect = selectTable categoryTable

filmCategorySelect :: Select (Field SqlInt8, Field SqlInt8)
filmCategorySelect = selectTable filmCategoryTable


runFilmSelect :: PGS.Connection
                   -> Select FilmInfoField
                   -> IO [FilmInfo]
runFilmSelect = runSelect


countFilms :: Select (Field SqlInt8)
countFilms = aggregate countStar filmSelect

filmsLongerThan :: Int32 -> Select FilmInfoField
filmsLongerThan len = do
  film <- filmSelect
  viaLateral restrict (filmLength film .>= toFields len)
  pure film

filmCategories :: Text -> Select (Field SqlText)
filmCategories filmTitle = do
  film <- filmSelect
  (ccatId, catName) <- categorySelect
  (fcfilmId, fccatId) <- filmCategorySelect
  viaLateral restrict $
        (title film .== toFields filmTitle)
    .&& (filmId film .== fcfilmId)
    .&& (ccatId .== fccatId)
  pure catName

runFilmsCategories :: PGS.Connection
                   -> [Text]
                   -> IO [(Text, [Text])]
runFilmsCategories conn = mapM runSingle
  where
    runSingle ttl = do
      cats <- runSelect conn $ filmCategories ttl
      pure (ttl, cats)

printFilmShort :: FilmInfo -> IO ()
printFilmShort fi = do
  print (title fi, releaseYear fi, filmLength fi, fromRating (rating fi) :: Text)

setRatingForFilm :: Rating -> Text -> Update Int64
setRatingForFilm fRating filmTitle =
  Update {
      uTable = filmTable
    , uUpdateWith = updateEasy (\film -> film {rating = toFields fRating})
    , uWhere      = \film -> title film .== toFields filmTitle
    , uReturning  = rCount
  }

newCategory :: Text -> Insert [Int64]
newCategory catName = Insert {
     iTable      = categoryTable
   , iRows       = [(Nothing, toFields catName)]
   , iReturning  = rReturning (\(id_, _) -> id_)
   , iOnConflict = Nothing
   }

applyCategory :: Int64 -> Int64 -> Insert Int64
applyCategory catId' filmId' = Insert {
     iTable      = filmCategoryTable
   , iRows       = [(toFields filmId', toFields catId')]
   , iReturning  = rCount
   , iOnConflict = Nothing
   }

filmByTitle :: Text -> Select FilmInfoField
filmByTitle filmTitle = do
  film <- filmSelect
  viaLateral restrict (title film .== toFields filmTitle)
  pure film


filmIdByTitle :: Text -> Select (Field SqlInt8)
filmIdByTitle filmTitle = do
  film <- filmSelect
  viaLateral restrict (title film .== toFields filmTitle)
  pure (filmId film)

newCatForFilm :: PGS.Connection -> Text -> Text -> IO Int64
newCatForFilm conn ncat filmTitle = do
  [ncatId] <- runInsert_ conn (newCategory ncat)
  [filmId'] <- runSelect conn (filmIdByTitle filmTitle)
  runInsert_ conn (applyCategory ncatId filmId')

randomCategory :: Text -> IO Text
randomCategory prefix = do
  n <- randomRIO (10000, 99999::Int)
  pure $ prefix <> T.pack (show n)


main :: IO ()
main = do
  conn <- PGS.connectPostgreSQL "host=localhost dbname=sakila_films"
  runFilmSelect conn filmSelect >>= mapM_ printFilmShort

  putStrLn "Total number of films:"
  runSelect conn countFilms >>= (print :: [Int64] -> IO ())

  putStrLn "Films of 185 minutes and longer:"
  runSelect conn (filmsLongerThan 185) >>= mapM_ printFilmShort

  putStrLn "Films categories:"
  runFilmsCategories conn ["KISSING DOLLS", "ALABAMA DEVIL"] >>= mapM_ print

  putStrLn "Setting rating for a film:"
  runUpdate_ conn (setRatingForFilm G "KISSING DOLLS") >>= print

  runSelect conn (filmByTitle "KISSING DOLLS") >>= mapM_ printFilmShort

  putStrLn "Apply random category to a film:"
  let film = "MODERN DORADO"
  newcat <- randomCategory "cat_"
  newCatForFilm conn newcat film >>= print
  runFilmsCategories conn [film] >>= mapM_ print

  putStrLn "OK"
