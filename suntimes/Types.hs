{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text
import Data.ByteString
import GHC.Generics
import Data.Time
import Data.Aeson
import Dhall
import Control.Exception.Safe


type Address = Text

data When = Now | On Day
  deriving Show

data GeoCoords = GeoCoords { lat :: Text,
                             lon :: Text }
  deriving (Show, Generic)

instance FromJSON GeoCoords

data SunTimes dt = SunTimes { sunrise :: dt,
                              sunset :: dt }
  deriving (Show, Generic)

instance FromJSON dt => FromJSON (SunTimes dt)

data WebAPIAuth = WebAPIAuth { timeZoneDBkey :: Text,
                               email :: Text,
                               agent :: Text}
  deriving (Generic, Show)

instance Interpret WebAPIAuth
