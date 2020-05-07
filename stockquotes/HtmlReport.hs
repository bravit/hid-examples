{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HtmlReport (htmlReport) where

import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import QuoteData
import StatReport (showStatEntryValue, showPrice)
import Statistics
import Fmt (pretty)

htmlReport :: (Functor t, Foldable t) =>
              String -> t QuoteData -> StatInfo -> [FilePath] -> ByteString
htmlReport title quotes si images = renderHtml $ docTypeHtml $ do
     H.head $ do
       H.title $ string title
       H.style style
     body $ do
       renderCharts images
       renderStatInfo si
       renderData quotes
  where
    style = "table {border-collapse: collapse}" <>
            "td, th {border: 1px solid black; padding: 3px}"

    renderCharts [] = pure ()
    renderCharts images = do
      h1 "Charts"
      traverse_ ((img!).src.toValue) images

    renderStatInfo [] = pure ()
    renderStatInfo si@((_, ses):_) = do
      h1 "Statistics Report"
      table $ do
         thead $ tr $ traverse_ th
               $ "Quotes Field" : [text $ pretty $ stat s | s <- ses]
         tbody $ traverse_ statData2TR si

    statData2TR (qf, entries) = tr $ do
      td $ string $ show qf
      traverse_ (td.text.showStatEntryValue) entries

    renderData quotes = do
      h1 "Stock Quotes Data"
      table $ do
         thead $ tr
               $ traverse_ th ["Day", "Volume", "Close", "Open", "High", "Low"]
         tbody $ traverse_ quoteData2TR quotes

    quoteData2TR (QuoteData {..}) = tr $ do
      td $ string $ show day
      td $ string $ show volume
      traverse_ (td.text.showPrice) [close, open, high, low]
