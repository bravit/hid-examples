{-# LANGUAGE QuasiQuotes #-}

module Statements where

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

import Data.Profunctor
import Data.Bifunctor (bimap)
import Data.Int
import Data.Maybe (isJust)
import Data.Vector (Vector)
import Data.Text (Text)

import FilmInfo.Data

fiFromTuple :: (Int64, Text, Text, Int32, Text)
                -> FilmInfo
fiFromTuple (i, t, d, l, r) = FilmInfo {
    filmId = FilmId i
  , title = t
  , description = Just d
  , filmLength = FilmLength l
  , rating = toMaybeRating r
  }

fromFilmId :: FilmId -> Int64
fromFilmId (FilmId a) = a

fromCatId :: CatId -> Int64
fromCatId (CatId a) = a

allFilms :: Statement () (Vector FilmInfo)
allFilms = rmap (fmap fiFromTuple)
  [TH.vectorStatement|
     SELECT film_id :: int8, title :: text,
            description :: text,
            length :: int4, rating :: text
     FROM film
  |]

countFilms :: Statement () Int64
countFilms =
  [TH.singletonStatement|
     SELECT count(*) :: int8 FROM film
  |]

findFilm :: Statement Text (Maybe FilmInfo)
findFilm = rmap (fmap fiFromTuple)
  [TH.maybeStatement|
     SELECT film_id :: int8, title :: text,
            description :: text,
            length :: int4, rating :: text
     FROM film
     WHERE title = $1::text
  |]

filmsLonger :: Statement Int32 (Vector FilmInfo)
filmsLonger = rmap (fmap fiFromTuple)
  [TH.vectorStatement|
     SELECT film_id :: int8, title :: text,
            description :: text,
            length :: int4, rating :: text
     FROM film
     WHERE length >= $1::int4
  |]

filmCategories :: Statement Text (Vector Text)
filmCategories =
  [TH.vectorStatement|
     SELECT category.name :: text FROM film
     JOIN film_category USING (film_id)
     JOIN category USING (category_id)
     WHERE title = $1 :: text
  |]

setRating :: Statement (Rating, Text) Int64
setRating = lmap (first' fromRating)
  [TH.rowsAffectedStatement|
    UPDATE film SET rating = $1 :: text :: mpaa_rating
    WHERE title = $2 :: text
  |]

filmIdByTitle :: Statement Text (Maybe FilmId)
filmIdByTitle = rmap (fmap FilmId)
  [TH.maybeStatement|
     SELECT film_id::int8 FROM film WHERE title=$1::text
  |]

catIdByName :: Statement Text (Maybe CatId)
catIdByName = rmap (fmap CatId)
  [TH.maybeStatement|
     SELECT category_id::int8 FROM category WHERE name=$1::text
  |]

newCategory :: Statement Text CatId
newCategory = rmap CatId
  [TH.singletonStatement|
    INSERT INTO category (name) VALUES ($1::text)
    RETURNING category_id::int8
  |]

isAssigned :: Statement (CatId, FilmId) Bool
isAssigned = lmap (bimap fromCatId fromFilmId) $ rmap isJust
  [TH.maybeStatement|
     SELECT category_id::int8 FROM film_category
     WHERE category_id=$1::int8 AND film_id=$2::int8
  |]

assignCategory :: Statement (CatId, FilmId) Int64
assignCategory = lmap (bimap fromCatId fromFilmId) $
  [TH.rowsAffectedStatement|
     INSERT INTO film_category (category_id, film_id)
     VALUES ($1::int8, $2::int8)
  |]

unassignCategory :: Statement (Text, Text) Int64
unassignCategory =
  [TH.rowsAffectedStatement|
     DELETE FROM film_category
     USING film, category
     WHERE category.name=$1::text AND
           film.title=$2::text AND
           film_category.film_id=film.film_id AND
           film_category.category_id=category.category_id
  |]
