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
  unless silent $ putStr textRpt
  when chart $ plotChart title quotes chartFname
  saveHtml htmlFile htmlRpt
 where
   statInfo' = statInfo quotes
   textRpt = textReport statInfo'
   htmlRpt = htmlReport title quotes statInfo' [chartFname | chart]

   withCompany prefix = if company /= ""
                        then prefix ++ company
                        else ""

   chartFname = "chart" ++ withCompany "_" ++ ".svg"
   title = "Historical Quotes" ++ withCompany " for "

   saveHtml Nothing _ = pure ()
   saveHtml (Just fname) html = BL.writeFile fname html

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

main :: IO ()
main = cmdLineParser >>= work
