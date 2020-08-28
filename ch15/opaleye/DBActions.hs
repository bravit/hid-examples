{-# LANGUAGE FlexibleContexts #-}

module DBActions where

import Opaleye
import Database.PostgreSQL.Simple (Connection)

import Data.Text (Text)
import Data.Int

import FilmInfo
import Queries

allFilms :: Connection -> IO [FilmInfo]
allFilms conn = runSelect conn filmSelect

totalFilmsNumber :: Connection -> IO Int64
totalFilmsNumber conn = do
  [cnt] <- runSelect conn countFilms
  pure cnt

findFilm :: Connection -> Text -> IO FilmInfo
findFilm conn ttl = do
  [film] <- runSelect conn $ filmByTitle ttl
  pure film

filmsLonger :: Connection -> FilmLength -> IO [FilmInfo]
filmsLonger conn len = runSelect conn $ filmsLongerThan len

filmsCategories :: Connection -> [Text] -> IO [FilmCategories]
filmsCategories conn = mapM runSingle
  where
    runSingle ttl = do
      film <- findFilm conn ttl
      cats <- runSelect conn $ filmCategories ttl
      pure $ FilmCategories film cats

findOrAddCategory :: Connection -> Text -> IO [CatId]
findOrAddCategory conn catName = do
  cats <- runSelect conn (catIdByName catName)
  case cats of
    [] -> runInsert_ conn (newCategory catName)
    (cid:_) -> pure [cid]

isAssigned :: Connection -> CatId -> FilmId -> IO Bool
isAssigned conn cid fid = do
  cats <- runSelect conn (findAssigned cid fid) :: IO [CatId]
  pure (length cats > 0)

runAssignCategory :: Connection -> Text -> Text -> IO Int64
runAssignCategory conn catName filmTitle = do
    [cid] <- findOrAddCategory conn catName
    filmIds <- runSelect conn (filmIdByTitle filmTitle)
    case filmIds of
      [] -> pure 0
      (fid:_) -> go cid fid
  where
    go cid fid = do
      b <- isAssigned conn cid fid
      case b of
        True -> pure 0
        False -> runInsert_ conn (assignCategory cid fid)

runUnassignCategory :: Connection -> Text -> Text -> IO Int64
runUnassignCategory conn catName filmTitle = do
  catIds <- runSelect conn (catIdByName catName)
  filmIds <- runSelect conn (filmIdByTitle filmTitle)
  case (catIds, filmIds) of
    ([cid], [fid]) -> runDelete_ conn (unassignCategory cid fid)
    _ -> pure 0

setRating :: Connection -> Rating -> Text -> IO Int64
setRating conn r filmTitle = runUpdate_ conn (setRatingForFilm r filmTitle)
