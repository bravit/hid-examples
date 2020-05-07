{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module QuoteData where

import Data.Fixed (HasResolution (..), Fixed)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Safe (readDef)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, FromField (..))

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
  deriving (Generic, FromNamedRecord)

instance FromField Fixed4 where
  parseField = pure . readDef 0 . unpack

instance FromField Day where
  parseField = parseTimeM False defaultTimeLocale "%Y-%m-%d" . unpack

data QField = Open | Close | High | Low | Volume
  deriving (Show, Enum, Bounded)

field2fun :: QField -> QuoteData -> Fixed4
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = volume
