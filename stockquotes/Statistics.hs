module Statistics (Statistic (..), StatEntry (..),
                   StatQFieldData, StatInfo, statInfo) where

import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Time (diffDays)

import QuoteData

data Statistic = Mean | Min | Max | Days
  deriving (Show, Eq, Enum, Bounded)

data StatEntry = StatEntry {
    stat :: Statistic,
    qfield :: QField,
    value :: Double
  }

type StatQFieldData = (QField, [StatEntry])
type StatInfo = [StatQFieldData]

mean xs = sum xs / fromIntegral (length xs)

daysBetween qf quotes = fromIntegral $ abs $ diffDays dMinQuote dMaxQuote
  where
    cmp = comparing (field2fun qf)
    dMinQuote = day $ minimumBy cmp quotes
    dMaxQuote = day $ maximumBy cmp quotes

funcByField func qf = func . fmap (field2fun qf)

computeStatistic Mean = funcByField mean
computeStatistic Min = funcByField minimum
computeStatistic Max = funcByField maximum
computeStatistic Days = daysBetween

statInfo :: (Functor t, Foldable t) => t QuoteData -> StatInfo
statInfo quotes = map stQFData [minBound .. maxBound]
  where
    stQFData qf = (qf, [ StatEntry st qf v |
                           st <- [minBound .. maxBound],
                           let v = computeStatistic st qf quotes ])
