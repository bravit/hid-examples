{-# LANGUAGE RecordWildCards #-}

module StatReport where

import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Time (diffDays)
import Fmt
import Colonnade

import QuoteData

decimalPlacesFloating = 2

data StatValue = StatValue {
    decimalPlaces :: Int,
    value :: Double
  }

instance Buildable StatValue where
  build sv = fixedF (decimalPlaces sv) (value sv)

data StatEntry = StatEntry {
    qfield :: QField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int
  }

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

computeMinMaxDays :: (Ord a, Foldable t, Num c) =>
                     (QuoteData -> a) -> t QuoteData -> (a, a, c)
computeMinMaxDays get quotes = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp quotes
    maxQ = maximumBy cmp quotes
    days = fromIntegral $ abs $ diffDays (day minQ) (day maxQ)

statInfo :: (Functor t, Foldable t) => t QuoteData -> [StatEntry]
statInfo quotes = fmap qFieldStatInfo [minBound .. maxBound]
  where
    decimalPlacesByQField Volume = 0
    decimalPlacesByQField _ = decimalPlacesFloating

    qFieldStatInfo qfield =
      let
        (mn, mx, daysBetweenMinMax) =
              computeMinMaxDays (field2fun qfield) quotes
        decPlaces = decimalPlacesByQField qfield
        meanVal = StatValue decimalPlacesFloating
                            (mean $ fmap (field2fun qfield) quotes)
        minVal = StatValue decPlaces mn
        maxVal = StatValue decPlaces mx
      in StatEntry {..}

asciiReport :: [StatEntry] -> String
asciiReport = ascii colStats
  where
    colStats = mconcat
      [ headed "Quote Field" (show . qfield)
      , headed "Mean" (pretty . meanVal)
      , headed "Min" (pretty . minVal)
      , headed "Max" (pretty . maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      ]

showPrice :: Double -> Builder
showPrice = fixedF decimalPlacesFloating
