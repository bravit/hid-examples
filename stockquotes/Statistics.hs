{-# LANGUAGE
      FlexibleInstances,
      OverloadedStrings,
      DeriveAnyClass
#-}

module Statistics (statReport) where

import Data.Ord (comparing)
import Data.Foldable
import Data.Fixed
import Data.Time
import qualified Data.Text as T
import Fmt

import BoundedEnum
import QuoteData

data Statistic = Mean | Min | Max | Days
  deriving (Show, Eq, Enum, Bounded, BoundedEnum)

instance Buildable Statistic where
  build Mean = "Mean"
  build Min = "Minimum"
  build Max = "Maximum"
  build Days = "Days between Min/Max"

type StatElement = (Statistic, QField, Fixed4)

instance Buildable StatElement where
  build (st, qf, val) = ""+|st|+": "+|value|+""
    where
      value = showFixed (removeTrailing st qf) val
      removeTrailing Days _ = True
      removeTrailing Min Volume = True
      removeTrailing Max Volume = True
      removeTrailing _ _ = False

type StatQFieldData = (QField, [StatElement])

instance Buildable StatQFieldData where
  build (qf, stats) = nameF ("Statistics for " +||qf||+"") $ unlinesF stats

daysBetween qf quotes = fromIntegral $ abs $ diffDays dMinQuote dMaxQuote
  where
    cmp = comparing (field2fun qf)
    dMinQuote = day $ minimumBy cmp quotes
    dMaxQuote = day $ maximumBy cmp quotes

funcByField func qf = func . fmap (field2fun qf)

mean xs = sum xs / fromIntegral (length xs)

computeStatistic ::  (Functor t, Foldable t) =>
                     Statistic -> QField -> t QuoteData -> Fixed4
computeStatistic Days = daysBetween
computeStatistic Mean = funcByField mean
computeStatistic Min = funcByField minimum
computeStatistic Max = funcByField maximum

statReport :: (Functor t, Foldable t) => t QuoteData -> T.Text
statReport quotes = fmt $ unlinesF $ map stQFData range
  where 
    stQFData qf = (qf, [(st, qf, computeStatistic st qf quotes) | st <- range])
