{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Tables where

import Data.Profunctor
import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Opaleye

import Data.Text (Text)

import FilmInfo.Data
import FilmInfo.FromField()

data PGRating

instance DefaultFromField PGRating Rating where
  defaultFromField = fieldQueryRunnerColumn

instance pgf ~ FieldNullable PGRating => Default ToFields Rating pgf where
  def = dimap fromRating (unsafeCast "mpaa_rating")
              (def :: ToFields Text (Field PGText))

makeAdaptorAndInstance "pFilmId" ''FilmId'
makeAdaptorAndInstance "pFilmLength" ''FilmLength'
makeAdaptorAndInstance "pFilmInfo" ''FilmInfo'

type FilmIdField = FilmId' (Field SqlInt4)
type OptionalFilmIdField = FilmId' (Maybe (Field SqlInt4))

type FilmLengthField = FilmLength' (Field SqlInt4)

type FilmInfoFieldWrite =
  FilmInfo' OptionalFilmIdField (Field SqlText) (FieldNullable SqlText)
            FilmLengthField (FieldNullable PGRating)

type FilmInfoField =
  FilmInfo' FilmIdField (Field SqlText) (FieldNullable SqlText)
            FilmLengthField (FieldNullable PGRating)

filmTable :: Table FilmInfoFieldWrite FilmInfoField
filmTable =
  table "film"
        (pFilmInfo FilmInfo {
              filmId = pFilmId $ FilmId $ tableField "film_id"
            , title = tableField "title"
            , description = tableField "description"
            , filmLength = pFilmLength $ FilmLength $ tableField "length"
            , rating = tableField "rating"
            })

makeAdaptorAndInstance "pCatId" ''CatId'

type CatIdField = CatId' (Field SqlInt4)
type OptionalCatIdField = CatId' (Maybe (Field SqlInt4))

categoryTable :: Table (OptionalCatIdField, Field SqlText)
                       (CatIdField, Field SqlText)
categoryTable =
  table "category"
        (p2 ( pCatId $ CatId $ tableField "category_id"
            , tableField "name"))

filmCategoryTable :: Table (FilmIdField, CatIdField)
                           (FilmIdField, CatIdField)
filmCategoryTable =
  table "film_category"
        (p2 ( pFilmId $ FilmId $ tableField "film_id"
            , pCatId $ CatId $ tableField "category_id"))
