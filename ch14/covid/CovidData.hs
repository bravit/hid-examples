{-# LANGUAGE TemplateHaskell #-}

module CovidData where

import Data.Time (Day)
import Data.Text (Text)
import Data.Map (Map)
import Control.Lens
import Data.ByteString (ByteString)

data CountryData = CountryData {
    _iso_code :: ByteString,
    _continent :: Text,
    _name :: Text,
    _days :: [(Day, DayInfo)],
    _stat :: Maybe CountryStat
  }
  deriving Show

data DayInfo = DayInfo {
    _cases :: DayCases,
    _deaths :: DayDeaths
  }
  deriving Show

data DayCases = DayCases {
    _total_cases :: Int,
    _new_cases :: Int
  }
  deriving Show

data DayDeaths = DayDeaths {
    _total_deaths :: Int,
    _new_deaths :: Int
  }
  deriving Show

data CountryStat = CountryStat {
    _population :: Int,
    _population_density :: Double
  }
  deriving Show

makeLenses ''CountryData
makeLenses ''DayInfo
makeLenses ''DayCases

