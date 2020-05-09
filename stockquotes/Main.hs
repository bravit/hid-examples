{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)

import QuoteData
import Charts
import StatReport
import HtmlReport
import Params

generateReports :: (Functor t, Foldable t) =>
                   Params -> t QuoteData -> IO ()
generateReports Params {..} quotes = do
  unless silent $ putStr $ asciiReport statInfo'
  when chart $ plotChart title quotes fname_chart
  genHTMLReport html
 where
   statInfo' = statInfo quotes

   withCompany "" pref = ""
   withCompany company pref = pref ++ company

   img_suffix = withCompany company "_" ++ ".svg"
   fname_chart = "chart" ++ img_suffix
   title = "Historical Quotes" ++ withCompany company " for "

   genHTMLReport Nothing = pure ()
   genHTMLReport (Just fname_html) =
     BL.writeFile fname_html
     $ htmlReport title quotes statInfo' [fname_chart | chart]

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

main :: IO ()
main = cmdLineParser >>= work
