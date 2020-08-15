{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module CovidData where

import Data.Time (Day)
import Data.Text (Text)
import TextShow
import Control.Lens
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M

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

data DayCases = DayCases {
    _total_cases :: Int,
    _new_cases :: Int
  }

data DayDeaths = DayDeaths {
    _total_deaths :: Int,
    _new_deaths :: Int
  }

data CountryStat = CountryStat {
    _population :: Int,
    _population_density :: Maybe Double
  }

data AccumulatedStat = AccumulatedStat {
    _acc_population :: Int,
    _acc_total_cases :: Int,
    _acc_total_deaths :: Int
  }

makeLenses ''CountryData
makeLenses ''DayInfo
makeLenses ''DayCases
makeLenses ''DayDeaths
makeLenses ''CountryStat
makeLenses ''AccumulatedStat

withDaysAndTotals :: CountryData -> [(Day, DayInfo)] -> CountryData
withDaysAndTotals countryData ds =
    withDays & current_total_deaths .~ ctDeaths
             & current_total_cases .~ ctCases
  where
    withDays = countryData & days %~ (++ ds)
    ctDeaths = maxOfDays (deaths . total_deaths) withDays
    ctCases = maxOfDays (cases . total_cases) withDays

    maxOfDays what = maximum1Of (days . folded . _2 . what)

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

fromCountryData :: CountryData -> AccumulatedStat
fromCountryData cd =
  AccumulatedStat (cd ^. stat . population)
                  (cd ^. current_total_cases)
                  (cd ^. current_total_deaths)

considerCountry :: Map Text AccumulatedStat
                   -> CountryData
                   -> Map Text AccumulatedStat
considerCountry stats cd =
      M.insertWith (<>) (cd ^. continent) (fromCountryData cd) stats
--   stats &
--      let new = fromCountryData cd
--      in at (cd ^. continent) %~ Just . maybe new (<> new)

worldStats :: Map Text AccumulatedStat -> AccumulatedStat
worldStats = M.foldl' (<>) mempty

instance TextShow (Map Text AccumulatedStat) where
  showb stats = M.foldlWithKey' withEntry "" stats
    where
      withEntry b nm st = b <> fromText nm <> "/" <> showb st <> "\n"
