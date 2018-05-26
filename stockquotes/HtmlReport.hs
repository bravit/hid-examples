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
import StatReport (showStatEntryValue)
import Statistics
import Fmt

htmlReport :: (Functor t, Foldable t) =>
              String -> t QuoteData -> StatInfo -> [FilePath] -> ByteString
htmlReport title quotes si images = renderHtml $ docTypeHtml $ do
     H.head $ do
       H.title $ string title
       H.style style
     body $ do
       renderDiagrams images
       renderStatInfo si
       renderData quotes
  where
    style = "table {border-collapse: collapse}" <>
            "td, th {border: 1px solid black; padding: 3px}"

    renderDiagrams [] = pure ()
    renderDiagrams images = do
      h1 "Diagrams"
      traverse_ ((img!).src.toValue) images

    renderStatInfo [] = pure ()
    renderStatInfo si@((_, ses):_) = do
      h1 "Statistics Report"
      table $ do
         thead $ tr $ traverse_ th
               $ "Quotes Field" : [text $ fmt $ build $ stat s | s <- ses]
         tbody $ traverse_ statData2TR si

    statData2TR (qf, entries) = tr $ do
      td $ string $ show qf
      traverse_ (td.string.showStatEntryValue) entries

    renderData quotes = do
      h1 "Stock Quotes Data"
      table $ do
         thead $ tr
               $ traverse_ th ["Day", "Close", "Volume", "Open", "High", "Low"]
         tbody $ traverse_ quoteData2TR quotes

    quoteData2TR QuoteData {..} = tr $ do
      td $ string $ show day
      traverse_ (td.string.show) [close, volume, open, high, low]
