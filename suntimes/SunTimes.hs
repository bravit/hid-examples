{-# LANGUAGE RecordWildCards, DeriveGeneric #-}

module SunTimes (getSunTimes) where

import Data.Aeson
import Network.HTTP.Req
import Data.Default
import Control.Exception.Safe
import qualified Data.Text as T
import Data.Time
import GHC.Generics

import TimeZones
import Types
import STExcept

newtype SunTimesWrapper dt = SunTimesWrapper {results :: SunTimes dt}
  deriving (Show, Generic)

instance FromJSON dt => FromJSON (SunTimesWrapper dt)

getSunTimesUTC :: GeoCoords -> When -> IO (SunTimes UTCTime)
getSunTimesUTC GeoCoords {..} w = handle rethrowReqException $ runReq def $ do
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

getSunTimes :: GeoCoords -> When -> WebAPIAuth -> IO (SunTimes ZonedTime)
getSunTimes gc@GeoCoords {..} d wauth = do
    SunTimes {..} <- getSunTimesUTC gc d `catch` noTimeHandler
    ltz <- lookupTimeZone gc sunrise wauth `catchAny` (const $ pure utc)
    return $ SunTimes (utcToZonedTime ltz sunrise)
                      (utcToZonedTime ltz sunset)
  where
    noTimeHandler :: MonadThrow m => SunInfoException -> m a
    noTimeHandler (ServiceAPIError _) = throw (UnknownTime gc)
    noTimeHandler e = throw e
