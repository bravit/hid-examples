{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport (showStatEntryValue, showPrice) where

import Fmt
import Data.Text (Text)

import QuoteData
import Statistics

pricePlaces = 4

instance Buildable Statistic where
  build Mean = "Mean"
  build Min = "Minimum"
  build Max = "Maximum"
  build Days = "Days between Min/Max"

statEntryValueBuilder :: StatEntry -> Builder
statEntryValueBuilder StatEntry {..} =
    fixedF (decimalPlaces stat qfield) value
  where
    decimalPlaces Days _ = 0
    decimalPlaces Min Volume = 0
    decimalPlaces Max Volume = 0
    decimalPlaces _ _ = pricePlaces

showStatEntryValue :: StatEntry -> Text
showStatEntryValue = fmt . statEntryValueBuilder

showPrice :: Double -> Text
showPrice = fmt . fixedF pricePlaces

instance Buildable StatEntry where
  build se@StatEntry {..} = build stat <> ": " <> statEntryValueBuilder se

instance Buildable StatQFieldData where
  build (qf, stats) = nameF ("Statistics for " +||qf||+"") $ unlinesF stats

instance Buildable StatInfo where
  build = unlinesF
