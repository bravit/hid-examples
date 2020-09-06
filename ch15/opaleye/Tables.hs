{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Tables where

import Data.Profunctor (dimap)
import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)

import Opaleye

import Data.Text (Text)

import FilmInfo.Data
import FilmInfo.FromField()

data PGRating

instance DefaultFromField PGRating Rating where
  defaultFromField = fromPGSFromField

instance pgf ~ FieldNullable PGRating => Default ToFields Rating pgf where
  def = dimap fromRating
              (unsafeCast "mpaa_rating")
              (def :: ToFields Text (Field PGText))

makeAdaptorAndInstanceInferrable "pFilmId" ''FilmId'
makeAdaptorAndInstanceInferrable "pFilmLength" ''FilmLength'
makeAdaptorAndInstanceInferrable "pFilmInfo" ''FilmInfo'

type FilmIdField = FilmId' (Field SqlInt4)
type FilmIdFieldWrite = FilmId' (Maybe (Field SqlInt4))

type FilmLengthField = FilmLength' (Field SqlInt4)

type FilmInfoField =
  FilmInfo'
    FilmIdField
    (Field SqlText)
    (FieldNullable SqlText)
    FilmLengthField
    (FieldNullable PGRating)

type FilmInfoFieldWrite =
  FilmInfo'
    FilmIdFieldWrite
    (Field SqlText)
    (FieldNullable SqlText)
    FilmLengthField
    (FieldNullable PGRating)


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

makeAdaptorAndInstanceInferrable "pCatId" ''CatId'

type CatIdField = CatId' (Field SqlInt4)
type CatIdFieldWrite = CatId' (Maybe (Field SqlInt4))

categoryTable :: Table (CatIdFieldWrite, Field SqlText)
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
