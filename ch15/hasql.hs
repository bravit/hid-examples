{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import Hasql.Transaction.Sessions
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc
import qualified Hasql.TH as TH



import Data.Int
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Profunctor
import System.Random
import Control.Monad (when)
import Control.Monad.Trans (liftIO)

data Rating = G | PG | PG13 | R | NC17
  deriving Show

data FilmInfo = FilmInfo {title :: Text, rating :: Maybe Rating, length :: Int64}
  deriving Show

ratingFromText :: Text -> Maybe Rating
ratingFromText "G" = Just G
ratingFromText "PG" = Just PG
ratingFromText "PG-13" = Just PG13
ratingFromText "R" = Just R
ratingFromText "NC-17" = Just NC17
ratingFromText _ = Nothing

ratingToText :: Rating -> Text
ratingToText G = "G"
ratingToText PG = "PG"
ratingToText PG13 = "PG-13"
ratingToText R = "R"
ratingToText NC17 = "NC-17"

fiFromTriple :: (Text, Text, Int64) -> FilmInfo
fiFromTriple (t, r, l) = FilmInfo t (ratingFromText r) l

countActorsStmt :: Statement () Int64
countActorsStmt =
  [TH.singletonStatement|
     SELECT count(*) :: int8 FROM actor
  |]

filmsLongerThanStmt :: Statement Int64 (Vector FilmInfo)
filmsLongerThanStmt = rmap (fmap fiFromTriple)
  [TH.vectorStatement|
     SELECT title :: text, rating :: text, length :: int8
     FROM film
     WHERE length >= $1::int8
  |]

filmCategoriesStmt :: Statement Text (Vector Text)
filmCategoriesStmt =
  [TH.vectorStatement|
     SELECT category.name :: text FROM film
     JOIN film_category USING (film_id)
     JOIN category USING (category_id)
     WHERE title = $1 :: text
  |]

setRatingForFilmStmt :: Statement (Rating, Text) Int64
setRatingForFilmStmt = lmap (first' ratingToText)
  [TH.rowsAffectedStatement|
    UPDATE film SET rating = $1 :: text :: mpaa_rating
    WHERE title = $2 :: text
  |]

newCategoryStmt :: Statement Text Int64
newCategoryStmt =
  [TH.singletonStatement|
    INSERT INTO category (name) VALUES ($1::text)
    RETURNING category_id::int8
  |]

applyCategoryStmt :: Statement (Int64, Int64) Int64
applyCategoryStmt =
  [TH.rowsAffectedStatement|
     INSERT INTO film_category (film_id, category_id)
     VALUES ($1::int8, $2::int8)
  |]

filmIdByTitleStmt :: Statement Text Int64
filmIdByTitleStmt =
  [TH.singletonStatement|
     SELECT film_id::int8 FROM film WHERE title=$1::text
  |]

countActors :: Session Int64
countActors = Session.statement () countActorsStmt

filmsLongerThan :: Int64 -> Session (Vector FilmInfo)
filmsLongerThan len = Session.statement len filmsLongerThanStmt

filmsCategories :: Vector Text -> Session (Vector (Text, Vector Text))
filmsCategories = V.mapM runSingle
  where
    runSingle ttl = (ttl, ) <$> Session.statement ttl filmCategoriesStmt

setRatingForFilm :: (Rating, Text) -> Session Int64
setRatingForFilm params =
  Session.statement params setRatingForFilmStmt

newCatForFilm :: (Text, Text) -> Session Int64
newCatForFilm (ncat, film) = transaction ReadCommitted Write $ do
  catId <- Transaction.statement ncat newCategoryStmt
  filmId <- Transaction.statement film filmIdByTitleStmt
  Transaction.statement (catId, filmId) applyCategoryStmt

randomCategory :: Text -> IO Text
randomCategory prefix = do
  n <- randomRIO (10000, 99999::Int)
  pure $ prefix <> T.pack (show n)

printAllFilms :: Session ()
printAllFilms = do
    Session.sql "BEGIN"
    Session.sql declareCursor
    fetchRowsLoop
    Session.sql "END"
  where
    declareCursor = "DECLARE films_cursor CURSOR FOR "
                    <> "SELECT title, rating, length FROM film;"
    decoder = Dec.rowVector $
      FilmInfo <$> (Dec.column . Dec.nonNullable) Dec.text
               <*> (fmap (>>= ratingFromText) . Dec.column .
                      Dec.nullable) Dec.text
               <*> (Dec.column . Dec.nonNullable) Dec.int8
    fetch = Session.statement () $
              Statement "FETCH FORWARD 10 FROM films_cursor"
                Enc.noParams decoder True
    fetchRowsLoop = do
      rows <- fetch
      when (not $ V.null rows) $ do
        liftIO (V.mapM_ print rows)
        fetchRowsLoop

main :: IO ()
main = do
    Right conn <- Connection.acquire connectionSettings

    putStrLn "Total number of actors:"
    Right cnt <- Session.run countActors conn
    print cnt

    putStrLn "Films of 185 minutes and longer:"
    Right v <- Session.run (filmsLongerThan 185) conn
    V.mapM_ print v

    putStrLn "Films categories:"
    let titles = ["KISSING DOLLS", "ALABAMA DEVIL"]
    Right vcats <- Session.run (filmsCategories $ V.fromList titles) conn
    V.mapM_ print vcats

    putStrLn "Setting rating for a film:"
    Right n <- Session.run (setRatingForFilm (G, "KISSING DOLLS")) conn
    print n

    putStrLn "Apply random category to a film:"
    let film = "MODERN DORADO"
    newcat <- randomCategory "cat_"
    Right n' <- Session.run (newCatForFilm (newcat, film)) conn
    print n'

    Right vcats' <- Session.run (filmsCategories $ V.fromList [film]) conn
    V.mapM_ print vcats'

    putStrLn "All films:"
    Right () <- Session.run printAllFilms conn
    pure ()
  where
    connectionSettings =
      Connection.settings "localhost" 5432 "" "" "sakila_films"
