{-# LANGUAGE RecordWildCards, DeriveGeneric #-}

module TimeZones (lookupTimeZone) where

import Data.Aeson
import Network.HTTP.Req
import Data.Default
import qualified Data.Text as T
import Data.Time
import GHC.Generics

import Types

data TimeZoneInfo = TimeZoneInfo { gmtOffset :: Int
                                 , abbreviation :: String
                                 , dst :: String  
                                 }  
  deriving (Show, Generic)

instance FromJSON TimeZoneInfo

timeZoneInfo2TimeZone :: TimeZoneInfo -> TimeZone
timeZoneInfo2TimeZone TimeZoneInfo {..} =
    (secondsToTimeZone gmtOffset) { timeZoneName = abbreviation
                                  , timeZoneSummerOnly = dst == "1"}
  where
    secondsToTimeZone s = minutesToTimeZone (s `div` 60)

lookupTimeZone :: GeoCoords -> UTCTime -> WebAPIAuth -> IO TimeZone
lookupTimeZone gc@GeoCoords {..} t wauth = do
    r <- runReq def $ req GET ep NoReqBody jsonResponse reqParams
    pure $ timeZoneInfo2TimeZone $ responseBody r
  where
    ep = http "api.timezonedb.com" /: "v2.1" /: "get-time-zone"
    reqParams = mconcat [ "key" =: timeZoneDBkey wauth
                        , "lat" =: lat
                        , "lng" =: lon
                        , "time" =: formatTime defaultTimeLocale "%s" t
                        , "format" =: ("json" :: T.Text)
                        , "fields" =: ("gmtOffset,abbreviation,dst" :: T.Text)
                        , "by" =: ("position" :: T.Text)
                        ]
