{-# LANGUAGE OverloadedStrings #-}

module FilmRating (Rating(..)) where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.ByteString
import qualified Data.ByteString.Char8 as B


data Rating = G | PG | PG13 | R | NC17
  deriving Show

toByteString :: Rating -> ByteString
toByteString G = "G"
toByteString PG = "PG"
toByteString PG13 = "PG-13"
toByteString R = "R"
toByteString NC17 = "NC-17"

fromByteString :: ByteString -> Maybe Rating
fromByteString "G" = Just G
fromByteString "PG" = Just PG
fromByteString "PG-13" = Just PG13
fromByteString "R" = Just R
fromByteString "NC-17" = Just NC17
fromByteString _ = Nothing

instance FromField Rating where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) =
    case fromByteString bs of
      Nothing -> returnError ConversionFailed f (B.unpack bs)
      Just r -> pure r

instance ToField Rating where
  toField r = Escape $ toByteString r
