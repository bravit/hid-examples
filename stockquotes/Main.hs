{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (toList)
import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Fmt (pretty)

import QuoteData
import Statistics
import StatReport
import Charts
import HtmlReport
import Params

generateReports :: (Functor t, Foldable t) =>
                   Params -> t QuoteData -> IO ()
generateReports Params {..} quotes = do
  unless no_text $ pretty statInfo'
  when chart $ plotChart title (toList quotes) fname_chart
  when html $ BL.writeFile fname_html
            $ htmlReport title quotes statInfo' images
 where
   statInfo' = statInfo quotes
   withCompany pref  = if company /= "" then pref ++ company else ""
   img_suffix = withCompany "_" ++ ".svg"
   fname_chart = "quotes" ++ img_suffix
   images = if chart then [fname_chart] else []
   fname_html = "report" ++ withCompany "_" ++ ".html"
   title = "Historical Quotes" ++ withCompany " for "

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

main :: IO ()
main = cmdLineParser >>= work
