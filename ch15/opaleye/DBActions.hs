{-# LANGUAGE FlexibleContexts #-}

module DBActions where

import Opaleye
import Database.PostgreSQL.Simple (Connection)

import Data.Text (Text)
import Data.Int

import FilmInfo
import qualified Queries as Q

allFilms :: Connection -> IO [FilmInfo]
allFilms conn = runSelect conn Q.filmSelect

totalFilmsNumber :: Connection -> IO Int64
totalFilmsNumber conn = do
  [cnt] <- runSelect conn Q.countFilms
  pure cnt

findFilm :: Connection -> Text -> IO FilmInfo
findFilm conn ttl = do
  [film] <- runSelect conn $ Q.findFilm ttl
  pure film

filmsLonger :: Connection -> FilmLength -> IO [FilmInfo]
filmsLonger conn len = runSelect conn $ Q.filmsLonger len

filmsCategories :: Connection -> [Text] -> IO [FilmCategories]
filmsCategories conn = mapM runSingle
  where
    runSingle ttl = do
      film <- findFilm conn ttl
      cats <- runSelect conn $ Q.filmCategories ttl
      pure $ FilmCategories film cats

setRating :: Connection -> Rating -> Text -> IO Int64
setRating conn r filmTitle = runUpdate_ conn (Q.setRating r filmTitle)

findOrAddCategory :: Connection -> Text -> IO [CatId]
findOrAddCategory conn catName = do
  cats <- runSelect conn (Q.catIdByName catName)
  case cats of
    [] -> runInsert_ conn (Q.newCategory catName)
    (cid:_) -> pure [cid]

isAssigned :: Connection -> CatId -> FilmId -> IO Bool
isAssigned conn cid fid = do
  cats <- runSelect conn (Q.findAssigned cid fid) :: IO [CatId]
  pure (length cats > 0)

assignCategory :: Connection -> Text -> Text -> IO Int64
assignCategory conn catName filmTitle = do
    [cid] <- findOrAddCategory conn catName
    filmIds <- runSelect conn (Q.filmIdByTitle filmTitle)
    case filmIds of
      [] -> pure 0
      (fid:_) -> go cid fid
  where
    go cid fid = do
      b <- isAssigned conn cid fid
      case b of
        True -> pure 0
        False -> runInsert_ conn (Q.assignCategory cid fid)

unassignCategory :: Connection -> Text -> Text -> IO Int64
unassignCategory conn catName filmTitle = do
  catIds <- runSelect conn (Q.catIdByName catName)
  filmIds <- runSelect conn (Q.filmIdByTitle filmTitle)
  case (catIds, filmIds) of
    ([cid], [fid]) -> runDelete_ conn (Q.unassignCategory cid fid)
    _ -> pure 0
