{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Text
import GHC.Generics
import Data.Time (Day)
import Data.Aeson

type Address = Text

data When = Now | On Day
  deriving Show

data GeoCoords = GeoCoords { lat :: Text,
                             lon :: Text }
  deriving (Show, Generic, FromJSON)

data SunTimes dt = SunTimes { sunrise :: dt,
                              sunset :: dt }
  deriving (Show, Generic, FromJSON)

data WebAPIAuth = WebAPIAuth { timeZoneDBkey :: Text,
                               email :: Text,
                               agent :: Text}
  deriving (Show, Generic, FromJSON)
