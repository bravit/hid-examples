{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module QuoteData where

import Data.Fixed
import Data.Time
import Safe
import GHC.Generics (Generic)
import Data.ByteString.Char8 (unpack)
import Data.Csv

import BoundedEnum
data E4
  
instance HasResolution E4 where
    resolution _ = 10000

type Fixed4 = Fixed E4

data QuoteData = QuoteData {
                   day :: Day,
                   close :: Fixed4,
                   volume :: Fixed4,
                   open :: Fixed4,
                   high :: Fixed4,
                   low :: Fixed4
                 }
  deriving (Generic, FromNamedRecord, DefaultOrdered)

instance FromField Fixed4 where
  parseField s = pure (readDef 0 $ unpack s)

instance FromField Day where
  parseField s = parseTimeM False defaultTimeLocale "%Y/%m/%d" (unpack s)

data QField = Open | Close | High | Low | Volume
  deriving (Show, Enum, Bounded, BoundedEnum)

field2fun :: QField -> QuoteData -> Fixed4
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = volume
