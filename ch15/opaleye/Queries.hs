module Queries where

import Opaleye

import Data.Text (Text)
import Data.Int

import FilmInfo.Data
import Tables

filmSelect :: Select FilmInfoField
filmSelect = selectTable filmTable

categorySelect :: Select (CatIdField, Field SqlText)
categorySelect = selectTable categoryTable

filmCategorySelect :: Select (FilmIdField, CatIdField)
filmCategorySelect = selectTable filmCategoryTable

countFilms :: Select (Field SqlInt8)
countFilms = aggregate countStar filmSelect

findFilm :: Text -> Select FilmInfoField
findFilm filmTitle = do
  film <- filmSelect
  viaLateral restrict (title film .== toFields filmTitle)
  pure film

filmsLonger :: FilmLength -> Select FilmInfoField
filmsLonger (FilmLength len) = do
    film <- filmSelect
    let FilmLength lenf = filmLength film
    viaLateral restrict (lenf .>= toFields len)
    pure film

filmCategories :: Text -> Select (Field SqlText)
filmCategories filmTitle = do
  film <- filmSelect
  (ccatId, catName) <- categorySelect
  (fcfilmId, fccatId) <- filmCategorySelect
  viaLateral restrict $
        (title film .== toFields filmTitle)
    .&& (filmId film .=== fcfilmId)
    .&& (ccatId .=== fccatId)
  pure catName

setRating :: Rating -> Text -> Update Int64
setRating fRating filmTitle =
  Update {
      uTable = filmTable
    , uUpdateWith = updateEasy (\film -> film {rating = toFields fRating})
    , uWhere      = \film -> title film .== toFields filmTitle
    , uReturning  = rCount
  }

filmIdByTitle :: Text -> Select FilmIdField
filmIdByTitle filmTitle = filmId <$> findFilm filmTitle

catIdByName :: Text -> Select CatIdField
catIdByName catName = do
  (catId, nm) <- categorySelect
  viaLateral restrict (nm .== toFields catName)
  pure catId

newCategory :: Text -> Insert [CatId]
newCategory catName = Insert {
     iTable      = categoryTable
   , iRows       = [(CatId Nothing, toFields catName)]
   , iReturning  = rReturning (\(id', _) -> id')
   , iOnConflict = Nothing
   }

findAssigned :: CatId -> FilmId -> Select CatIdField
findAssigned catId' filmId' = do
  (fid, cid) <- filmCategorySelect
  viaLateral restrict $ (fid .=== toFields filmId')
                    .&& (cid .=== toFields catId')
  pure cid

assignCategory :: CatId -> FilmId -> Insert Int64
assignCategory catId' filmId' = Insert {
     iTable      = filmCategoryTable
   , iRows       = [(toFields filmId', toFields catId')]
   , iReturning  = rCount
   , iOnConflict = Nothing
   }

unassignCategory :: CatId -> FilmId -> Delete Int64
unassignCategory catId' filmId' = Delete {
     dTable      = filmCategoryTable
   , dWhere =  \(fid, cid) -> (fid .=== toFields filmId')
                              .&& (cid .=== toFields catId')
   , dReturning  = rCount
   }
