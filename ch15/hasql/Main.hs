{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Hasql.Connection as Connection

import qualified Data.Vector as V
import qualified Data.Text.IO as T

import TextShow
import FilmInfo

import DBActions

main :: IO ()
main = do
  Right conn <- Connection.acquire connectionSettings

  printAllFilms conn
  allFilms conn >>= V.mapM_ printFilm . V.take 5

  T.putStr "\nTotal number of films: "
  totalFilmsNumber conn >>= printT

  let film = "MODERN DORADO"
  T.putStrLn "\nFilm information:"
  findFilm conn film >>= printT

  let len = FilmLength 185
  T.putStrLn $ "\nFilms of " <> showt len <> " and longer:"
  filmsLonger conn len >>= mapM_ printT

  let films = ["KISSING DOLLS", "ALABAMA DEVIL", film]
  T.putStrLn "\nFilms categories:"
  filmsCategories conn films >>= mapM_ printT

  let newRating = NC17
  T.putStr $ "\nSetting rating " <> fromRating newRating
              <>  " for a film (" <> film <> "): "
  setRating conn newRating film >>= printT
  findFilm conn film >>= printT

  let newCat = "Art"
  putStr "\nAssign category to a film: "
  runAssignCategory conn newCat film >>= print
  filmsCategories conn [film] >>= mapM_ printT

{-
  putStr "\nUnassign category from a film: "
  runUnassignCategory conn newCat film >>= print
  filmsCategories conn [film] >>= mapM_ printT
-}
 where
    connectionSettings =
      Connection.settings "localhost" 5432 "" "" "sakila_films"
