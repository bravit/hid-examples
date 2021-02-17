{-# LANGUAGE FlexibleContexts #-}

module DBActions where

import Opaleye
import Database.PostgreSQL.Simple (Connection)

import Data.Text (Text)
import Data.Int
import Data.Maybe (catMaybes, listToMaybe)

import FilmInfo.Data
import qualified Queries as Q

allFilms :: Connection -> IO [FilmInfo]
allFilms conn = runSelect conn Q.filmSelect

totalFilmsNumber :: Connection -> IO Int64
totalFilmsNumber conn = do
  [cnt] <- runSelect conn Q.countFilms
  pure cnt

findFilm :: Connection -> Text -> IO (Maybe FilmInfo)
findFilm conn = fmap listToMaybe . runSelect conn . Q.findFilm

filmsLonger :: Connection -> FilmLength -> IO [FilmInfo]
filmsLonger conn = runSelect conn . Q.filmsLonger

filmsCategories :: Connection -> [Text] -> IO [FilmCategories]
filmsCategories conn films = catMaybes <$> mapM runSingle films
  where
    runSingle filmTitle = do
      mfilm <- findFilm conn filmTitle
      case mfilm of
        Nothing -> pure Nothing
        Just film -> Just . FilmCategories film <$>
                     runSelect conn (Q.filmCategories filmTitle)

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

assignUnlessAssigned :: Connection -> CatId -> FilmId -> IO Int64
assignUnlessAssigned conn cid fid = do
  b <- isAssigned conn cid fid
  case b of
    True -> pure 0
    False -> runInsert_ conn (Q.assignCategory cid fid)

assignCategory :: Connection -> Text -> Text -> IO Int64
assignCategory conn catName filmTitle = do
    [cid] <- findOrAddCategory conn catName
    filmIds <- runSelect conn (Q.filmIdByTitle filmTitle)
    case filmIds of
      [] -> pure 0
      (fid:_) -> assignUnlessAssigned conn cid fid

unassignCategory :: Connection -> Text -> Text -> IO Int64
unassignCategory conn catName filmTitle = do
  catIds <- runSelect conn (Q.catIdByName catName)
  filmIds <- runSelect conn (Q.filmIdByTitle filmTitle)
  case (catIds, filmIds) of
    ([cid], [fid]) -> runDelete_ conn (Q.unassignCategory cid fid)
    _ -> pure 0
