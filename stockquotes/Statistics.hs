{-# LANGUAGE DeriveAnyClass #-}

module Statistics (Statistic(..),
                   StatEntry, StatQFieldData, StatInfo,
                   statInfo, statEntryStat, statEntryValue) where

import Data.Ord (comparing)
import Data.Foldable
import Data.Time

import BoundedEnum
import QuoteData

data Statistic = Mean | Min | Max | Days
  deriving (Show, Eq, Enum, Bounded, BoundedEnum)

type StatEntry = (Statistic, QField, Fixed4)
type StatQFieldData = (QField, [StatEntry])
type StatInfo = [StatQFieldData]

statEntryStat (st, _, _) = st
statEntryValue (_, _, v) = v

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

statInfo :: (Functor t, Foldable t) => t QuoteData -> StatInfo
statInfo quotes = map stQFData range
  where 
    stQFData qf = (qf, [(st, qf, computeStatistic st qf quotes) | st <- range])
