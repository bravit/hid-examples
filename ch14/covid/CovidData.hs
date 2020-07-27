{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CovidData where

import Data.Time (Day)
import Data.Text (Text)
import TextShow
import Control.Lens
import Data.ByteString (ByteString)

data CountryData = CountryData {
    _iso_code :: ByteString,
    _continent :: Text,
    _name :: Text,
    _current_total_cases :: Int,
    _current_total_deaths :: Int,
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


data AccumulatedStat = AccumulatedStat {
    _acc_population :: Int,
    _acc_total_cases :: Int,
    _acc_total_deaths :: Int
  }
  deriving (Show, Eq)



makeLenses ''CountryData
makeLenses ''DayInfo
makeLenses ''DayCases
makeLenses ''DayDeaths
makeLenses ''CountryStat
makeLenses ''AccumulatedStat


instance TextShow CountryData where
  showb cd = fromText (cd ^. name)
             <> " "
             <> showb (cd ^. stat . population)
             <> " "
             <> showb (cd ^. current_total_cases)
             <> " "
             <> showb (cd ^. current_total_deaths)

instance TextShow AccumulatedStat where
  showb (AccumulatedStat pop tc td) =
    showb pop <> "/" <> showb tc <> "/" <> showb td

instance Semigroup AccumulatedStat where
  (AccumulatedStat a b c) <> (AccumulatedStat a' b' c') =
      AccumulatedStat (a+a') (b+b') (c+c')

instance Monoid AccumulatedStat where
  mempty = AccumulatedStat 0 0 0
