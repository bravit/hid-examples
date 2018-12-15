{-# LANGUAGE RecordWildCards, DeriveGeneric #-}

module SunTimes (getSunTimes) where

import Data.Aeson
import Network.HTTP.Req
import Data.Default
import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Time
import GHC.Generics

import Types
import STExcept
import App

newtype SunTimesWrapper dt = SunTimesWrapper {results :: SunTimes dt}
  deriving (Show, Generic)

instance FromJSON dt => FromJSON (SunTimesWrapper dt)

getSunTimesUTC :: GeoCoords -> When -> MyApp (SunTimes UTCTime)
getSunTimesUTC GeoCoords {..} w = handle rethrowReqException $ liftIO $ runReq def $ do
    r <- req GET ep NoReqBody jsonResponse reqParams
    pure (results $ responseBody r)
  where      
    ep = https "api.sunrise-sunset.org" /: "json"
    reqParams =
      mconcat $ [ "lat" =: lat
                , "lng" =: lon
                , "formatted" =: (0 :: Int)
                ] ++ whenToOptions w
    whenToOptions Now = []
    whenToOptions (On day) = ["date" =: formatTime defaultTimeLocale "%Y-%m-%d" day]

getSunTimes :: GeoCoords -> When -> MyApp (SunTimes ZonedTime)
getSunTimes gc@GeoCoords {..} d = do
    SunTimes {..} <- getSunTimesUTC gc d `catch` noTimeHandler
    ltz <- lookupTimeZone gc sunrise `catchAny` (const $ pure utc)
    return $ SunTimes (utcToZonedTime ltz sunrise)
                      (utcToZonedTime ltz sunset)
  where
    noTimeHandler :: MonadThrow m => SunInfoException -> m a
    noTimeHandler (ServiceAPIError _) = throw (UnknownTime gc)
    noTimeHandler e = throw e

data TimeZoneInfo = TimeZoneInfo { gmtOffset :: Int
                                 , abbreviation :: String
                                 , dst :: String  
                                 }  
  deriving (Show, Generic)

instance FromJSON TimeZoneInfo

lookupTimeZone :: GeoCoords -> UTCTime -> MyApp TimeZone
lookupTimeZone gc@GeoCoords {..} t = do
    key <- asks timeZoneDBkey
    let
      ep = http "api.timezonedb.com" /: "v2.1" /: "get-time-zone"
      reqParams = mconcat [ "key" =: key
                          , "lat" =: lat
                          , "lng" =: lon
                          , "time" =: formatTime defaultTimeLocale "%s" t
                          , "format" =: ("json" :: T.Text)
                          , "fields" =: ("gmtOffset,abbreviation,dst" :: T.Text)
                          , "by" =: ("position" :: T.Text)
                          ]
    r <- liftIO $ runReq def $ req GET ep NoReqBody jsonResponse reqParams
    pure (timeZoneInfo2TimeZone $ responseBody r)
  where
    secondsToTimeZone s = minutesToTimeZone (s `div` 60)
    timeZoneInfo2TimeZone TimeZoneInfo {..} =
      (secondsToTimeZone gmtOffset) { timeZoneName = abbreviation
                                    , timeZoneSummerOnly = dst == "1"}
