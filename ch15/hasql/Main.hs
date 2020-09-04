{-# LANGUAGE OverloadedStrings #-}

import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection

import Prelude hiding (putStr, putStrLn)
import Data.Text.IO
import TextShow
import qualified Data.Vector as V

import FilmInfo.Data
import DBActions

demo :: Connection -> IO ()
demo conn = do
  printAllFilms conn
  allFilms conn >>= mapM_ printFilm . V.take 5

  putStr "\nTotal number of films: "
  totalFilmsNumber conn >>= printT

  let film = "MODERN DORADO"
  putStrLn "\nFilm information:"
  findFilm conn film >>= printT

  let len = FilmLength 185
  putStrLn $ "\nFilms of " <> showt len <> " and longer:"
  filmsLonger conn len >>= mapM_ printT

  let films = ["KISSING DOLLS", "ALABAMA DEVIL", film]
  putStrLn "\nFilms categories:"
  filmsCategories conn films >>= mapM_ printT

  let newRating = NC17
  putStr $ "\nSetting rating " <> fromRating newRating
              <>  " for a film (" <> film <> "): "
  setRating conn newRating film >>= printT
  findFilm conn film >>= printT

  let newCat = "Art"
  putStr "\nAssign category to a film: "
  assignCategory conn newCat film >>= print
  filmsCategories conn [film] >>= mapM_ printT

  putStr "\nUnassign category from a film: "
  unassignCategory conn newCat film >>= print
  filmsCategories conn [film] >>= mapM_ printT

main :: IO ()
main = do
  Right conn <- Connection.acquire connectionSettings
  demo conn
 where
    connectionSettings =
      Connection.settings "localhost" 5432 "" "" "sakila_films"
