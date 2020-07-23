{-# LANGUAGE TemplateHaskell #-}

module CovidData where

import Data.Time (Day)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Data.ByteString (ByteString)

data CountryData = CountryData {
    _iso_code :: ByteString,
    _continent :: Text,
    _name :: Text,
    _current_total_cases :: Int,
    _days :: [(Day, DayInfo)],
    _stat :: CountryStat
  }

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
    _population_density :: Maybe Double
  }
  deriving Show

makeLenses ''CountryData
makeLenses ''DayInfo
makeLenses ''DayCases
makeLenses ''CountryStat

instance Show CountryData where
  show cd = T.unpack (cd ^. name)
            <> " "
            <> show (last $ cd ^. days ^.. folded . _2 . cases . total_cases)
            <> " "
            <> show (cd ^. stat . population)
            <> " "
            <> show (cd ^. stat . population_density)
