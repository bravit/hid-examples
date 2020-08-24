{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

import Database.PostgreSQL.Simple

import GHC.Generics (Generic)
import Data.Int (Int64)
import FilmRating (Rating)
import qualified FilmRating as Rating

import System.Random

fetchSingle :: Applicative f => String -> [Only a] -> f a
fetchSingle _ [Only val] = pure val
fetchSingle what _ = error $ "Unexpected result: " ++ what

countActors :: Connection -> IO Int
countActors conn = do
  result <- query_ conn "SELECT count(*) FROM actor"
  fetchSingle "countActors" result

data FilmInfo = FilmInfo {title :: String, rating :: Rating, length :: Int}
  deriving (Generic, FromRow, Show)

filmsLongerThan :: Int -> Connection -> IO [FilmInfo]
filmsLongerThan len conn = query conn select (Only len)
  where
    select = "SELECT title, rating, length FROM film WHERE length >= ?"

filmsCategories :: [String] -> Connection -> IO [(String, [String])]
filmsCategories films conn = mapM runSingle films
  where
    select = "SELECT category.name FROM film"
             <> " JOIN film_category USING (film_id)"
             <> " JOIN category USING (category_id)"
             <> " WHERE title = ?"
    runSingle film = do
      cats <- query conn select (Only film)
      pure (film, map head cats)

setRatingForFilm :: Rating -> String -> Connection -> IO Int64
setRatingForFilm fRating film conn =
  execute conn "UPDATE film SET rating = ? WHERE title = ?" (fRating, film)

newCategory :: String -> Connection -> IO Int64
newCategory ncat conn =
    query conn insert (Only ncat) >>= fetchSingle "category_id"
  where
    insert = "INSERT INTO category (name) VALUES (?) RETURNING category_id"

applyCategory :: Int64 -> Int64 -> Connection -> IO Int64
applyCategory catId filmId conn =
  execute conn "INSERT INTO film_category (film_id, category_id) VALUES (?, ?)"
          (filmId, catId)

filmIdByTitle :: String -> Connection -> IO Int64
filmIdByTitle filmTitle conn =
  query conn "SELECT film_id FROM film WHERE title=?" (Only filmTitle)
  >>= fetchSingle "film_id"

newCatForFilm :: String -> String -> Connection -> IO Int64
newCatForFilm ncat film conn = do
  catId <- newCategory ncat conn
  filmId <- filmIdByTitle film conn
  applyCategory catId filmId conn

randomCategory :: String -> IO String
randomCategory prefix = do
  n <- randomRIO (10000, 99999::Int)
  pure $ prefix ++ show n

printAllFilms :: Connection -> IO ()
printAllFilms conn = forEach_ conn select (print :: FilmInfo -> IO ())
  where
    select = "SELECT title, rating, length FROM film"

main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost dbname=sakila_films"

  putStrLn "Total number of actors:"
  countActors conn >>= print

  putStrLn "Films of 185 minutes and longer:"
  filmsLongerThan 185 conn >>= mapM_ print

  putStrLn "Films categories:"
  filmsCategories ["KISSING DOLLS", "ALABAMA DEVIL"] conn >>= mapM_ print

  putStrLn "Setting rating for a film:"
  setRatingForFilm Rating.G "KISSING DOLLS" conn >>= print

  putStrLn "Apply random category to a film:"
  let film = "MODERN DORADO"
  newcat <- randomCategory "cat_"
  withTransaction conn (newCatForFilm newcat film conn) >>= print
  filmsCategories [film] conn >>= mapM_ print

  putStrLn "All films:"
  printAllFilms conn
