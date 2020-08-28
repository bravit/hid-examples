{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DBActions where

import Hasql.Connection (Connection)
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc

import Data.Int
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)

import qualified Statements as Stmt
import FilmInfo

allFilms :: Connection -> IO (Vector FilmInfo)
allFilms conn = do
  Right v <- Session.run (Session.statement () Stmt.allFilms) conn
  pure v

totalFilmsNumber :: Connection -> IO Int64
totalFilmsNumber conn = do
  Right cnt <- Session.run (Session.statement () Stmt.countFilms) conn
  pure cnt

findFilm :: Connection -> Text -> IO FilmInfo
findFilm conn ttl = do
  Right fi <- Session.run (Session.statement ttl Stmt.findFilm) conn
  pure fi

filmsLonger :: Connection -> FilmLength -> IO (Vector FilmInfo)
filmsLonger conn (FilmLength len) = do
  Right v <- Session.run (Session.statement len Stmt.filmsLonger) conn
  pure v

filmsCategories :: Connection -> [Text] -> IO [FilmCategories]
filmsCategories conn films = mapM runSingle films
  where
    runSingle ttl = do
      film <- findFilm conn ttl
      Right cats <-
        Session.run (Session.statement ttl Stmt.filmCategories) conn
      pure $ FilmCategories film (V.toList cats)

setRating :: Connection -> Rating -> Text -> IO Int64
setRating conn newRating film = do
  Right r <-
    Session.run (Session.statement (newRating, film) Stmt.setRating) conn
  pure r

findOrAddCategorySession :: Text -> Session CatId
findOrAddCategorySession catName = do
  cats <- Session.statement catName Stmt.catIdByName
  case cats of
    Nothing -> Session.statement catName Stmt.newCategory
    Just cid -> pure cid

runAssignCategorySession :: Text -> Text -> Session Int64
runAssignCategorySession catName filmTitle = do
  cid <- findOrAddCategorySession catName
  mFilmId <- Session.statement filmTitle Stmt.filmIdByTitle
  case mFilmId of
    Nothing -> pure 0
    Just fid -> go cid fid
 where
   go (CatId cid) (FilmId fid) = do
     b <- Session.statement (cid, fid) Stmt.isAssigned
     case b of
       True -> pure 0
       False -> Session.statement (cid, fid) Stmt.assignCategory

runAssignCategory :: Connection -> Text -> Text -> IO Int64
runAssignCategory conn catName filmTitle = do
  Right r <- Session.run (runAssignCategorySession catName filmTitle) conn
  pure r

printAllFilmsSession :: Session ()
printAllFilmsSession = do
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

printAllFilms :: Connection -> IO ()
printAllFilms conn = do
  Right () <- Session.run printAllFilmsSession conn
  pure ()
