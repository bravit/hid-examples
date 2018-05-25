{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport (statReport, showStatEntryValue) where

import Data.Fixed (showFixed)
import Data.Text (Text)
import Fmt

import QuoteData
import Statistics

instance Buildable Statistic where
  build Mean = "Mean"
  build Min = "Minimum"
  build Max = "Maximum"
  build Days = "Days between Min/Max"

showStatEntryValue :: StatEntry -> String
showStatEntryValue StatEntry {..} = showFixed (removeTrailing stat qfield) value
  where
    removeTrailing Days _ = True
    removeTrailing Min Volume = True
    removeTrailing Max Volume = True
    removeTrailing _ _ = False

instance Buildable StatEntry where
  build se@StatEntry {..} = ""+|stat|+": "+|showStatEntryValue se|+""

instance Buildable StatQFieldData where
  build (qf, stats) = nameF ("Statistics for " +||qf||+"") $ unlinesF stats

statReport :: StatInfo -> Text
statReport = fmt . unlinesF
