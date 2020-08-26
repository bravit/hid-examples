{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Convertible.Base

import System.Random

fetchSingle :: (Monad m, Convertible SqlValue a) =>
               String -> [[SqlValue]] -> m a
fetchSingle _ [[val]] = pure $ fromSql val
fetchSingle what _ = error $ "Unexpected result: " ++ what

countFilms :: Connection -> IO Int
countFilms conn = do
  result <- quickQuery conn "SELECT count(*) FROM film" []
  fetchSingle "countFilms" result

filmsLongerThan :: Int -> Connection -> IO [(String, Int)]
filmsLongerThan len conn = map res2pair <$> quickQuery conn select [toSql len]
  where
    select = "SELECT title, length FROM film WHERE length >= ?"

    res2pair [title, leng] = (fromSql title, fromSql leng)
    res2pair _ = error "Unexpected result"

filmsCategories :: [String] -> Connection -> IO [(String, [String])]
filmsCategories films conn = do
    stmt <- prepare conn select
    mapM (runSingle stmt) films
  where
    select = "SELECT category.name FROM film"
             <> " JOIN film_category USING (film_id)"
             <> " JOIN category USING (category_id)"
             <> " WHERE title = ?"
    runSingle stmt film = do
      _ <- execute stmt [toSql film]
      cats <- fetchAllRows' stmt
      pure (film, map (fromSql . head) cats)

availableRatings :: Connection -> IO [String]
availableRatings conn =
  map (fromSql . head)
  <$> quickQuery conn "SELECT unnest(enum_range(NULL::mpaa_rating))" []

setRatingForFilm :: String -> String -> Connection -> IO Integer
setRatingForFilm rating film conn = do
  ratings <- availableRatings conn
  if rating `notElem` ratings
    then pure 0
    else do
      res <- sRun conn "UPDATE film SET rating = ? WHERE title = ?"
           [Just rating, Just film]
      commit conn
      pure res

newCategory :: String -> Connection -> IO Int
newCategory ncat conn = do
  cnt <- run conn "INSERT INTO category (name) VALUES (?)" [toSql ncat]
  if cnt /= 1
    then error "Inserting category failed"
    else quickQuery conn "SELECT lastval()" [] >>= fetchSingle "category_id"

applyCategory :: Int -> Int -> Connection -> IO Integer
applyCategory catId filmId conn =
  run conn "INSERT INTO film_category (film_id, category_id) VALUES (?, ?)"
      [toSql filmId, toSql catId]

filmIdByTitle :: String -> Connection -> IO Int
filmIdByTitle title conn =
  quickQuery conn "SELECT film_id FROM film WHERE title=?" [toSql title]
  >>= fetchSingle "film_id"

newCatForFilm :: String -> String -> Connection -> IO Integer
newCatForFilm ncat film conn = do
  catId <- newCategory ncat conn
  filmId <- filmIdByTitle film conn
  applyCategory catId filmId conn

randomCategory :: String -> IO String
randomCategory prefix = do
  n <- randomRIO (10000, 99999::Int)
  pure $ prefix ++ show n

main :: IO ()
main = withPostgreSQL "host=localhost dbname=sakila_films"
       $ \conn -> handleSqlError $ do
  putStrLn "Total number of films:"
  countFilms conn >>= print

  putStrLn "Films of 185 minutes and longer:"
  filmsLongerThan 185 conn >>= mapM_ print

  putStrLn "Films categories:"
  filmsCategories ["KISSING DOLLS", "ALABAMA DEVIL"] conn >>= mapM_ print

  putStrLn "Setting rating for a film:"
  setRatingForFilm "G" "KISSING DOLLS" conn >>= print

  putStrLn "Apply random category to a film:"
  let film = "MODERN DORADO"
  newcat <- randomCategory "cat_"
  withTransaction conn (newCatForFilm newcat film) >>= print
  filmsCategories [film] conn >>= mapM_ print
