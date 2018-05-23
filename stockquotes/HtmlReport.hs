{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HtmlReport (htmlReport) where

import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Html5 as H 
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.Utf8

import QuoteData
import Statistics
import StatReport
import Fmt

htmlReport :: (Functor t, Foldable t) =>
              String -> t QuoteData -> StatInfo -> [FilePath] -> BL.ByteString
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

    statData2TR (qf, entries) = tr $ do
      td $ string $ show qf
      traverse_ (td.string.showStatEntryValue) entries

    renderStatInfo [] = pure ()
    renderStatInfo si@((_, ses):_) = do
      h1 "Statistics Report"
      table $ do
         thead $ traverse_ th
               $ "Quotes Field" : 
                 [ text $ fmt $ build $ statEntryStat s | s <- ses]
         tbody $ traverse_ statData2TR si

    quoteData2TR QuoteData {..} = tr $ do
      td $ string $ show day
      traverse_ (td.string.show) [close, volume, open, high, low] 

    renderData quotes = do
      h1 "Stock Quotes Data"
      table $ do
         thead $ traverse_ th ["Day", "Close", "Volume", "Open", "High", "Low"]
         tbody $ traverse_ quoteData2TR quotes
