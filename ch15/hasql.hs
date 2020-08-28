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
import Data.Maybe

import FilmInfo

fiFromTuple :: (Int64, Text, Text, Int32, Int32, Text)
                -> FilmInfo
fiFromTuple (i, t, d, y, l, r) = FilmInfo {
    filmId = i
  , title = t
  , description = d
  , releaseYear = y
  , filmLength = l
  , rating = fromJust $ toMaybeRating r
  }

countFilmsStmt :: Statement () Int64
countFilmsStmt =
  [TH.singletonStatement|
     SELECT count(*) :: int8 FROM film
  |]

filmsLongerThanStmt :: Statement Int32 (Vector FilmInfo)
filmsLongerThanStmt = rmap (fmap fiFromTuple)
  [TH.vectorStatement|
     SELECT film_id :: int8, title :: text,
            description :: text, release_year :: int4,
            length :: int4, rating :: text
     FROM film
     WHERE length >= $1::int4
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
setRatingForFilmStmt = lmap (first' fromRating)
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

countFilms :: Session Int64
countFilms = Session.statement () countFilmsStmt

filmsLongerThan :: Int32 -> Session (Vector FilmInfo)
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
  filmId' <- Transaction.statement film filmIdByTitleStmt
  Transaction.statement (filmId', catId) applyCategoryStmt

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
      (,,) <$> (Dec.column . Dec.nonNullable) Dec.text
               <*> (fmap (>>= toMaybeRating) . Dec.column .
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


    putStrLn "All films:"
    Right () <- Session.run printAllFilms conn

    putStrLn "Total number of films:"
    Right cnt <- Session.run countFilms conn
    print cnt

    putStrLn "Films of 185 minutes and longer:"
    Right v <- Session.run (filmsLongerThan 185) conn
    V.mapM_ printFilmShort v

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

    pure ()
  where
    connectionSettings =
      Connection.settings "localhost" 5432 "" "" "sakila_films"
