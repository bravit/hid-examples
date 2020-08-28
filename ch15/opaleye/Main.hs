{-# LANGUAGE OverloadedStrings #-}

import qualified Database.PostgreSQL.Simple as PGS

import qualified Data.Text.IO as T
import TextShow

import FilmInfo
import DBActions

main :: IO ()
main = do
  conn <- PGS.connectPostgreSQL connString

  allFilms conn >>= mapM_ printFilm . take 5

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

  putStr "\nUnassign category from a film: "
  runUnassignCategory conn newCat film >>= print
  filmsCategories conn [film] >>= mapM_ printT
 where
   connString = "host=localhost dbname=sakila_films"
