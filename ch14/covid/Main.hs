{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Either
import Data.Function (on)
import Control.Lens

import Streaming
import Streaming.Zip
import qualified Streaming.Prelude as S
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Streaming.Char8 as C
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as ABS

import CovidData
import CovidCSVParser

-- https://ourworldindata.org/coronavirus-source-data

readCovidData :: IO ()
readCovidData =
  C.readFile "data/owid-covid-data.csv.gz"
--  C.readFile "data/a.csv.gz"
  & gunzip
  & ABS.parsed countryCodeWithRest
  & void
  & S.drop 1
  & S.groupBy ((==) `on` fst)
  & S.mapped toCountryData
  & S.catMaybes
  & S.map (days %~ reverse)
--  & S.map f
--  & S.sum
  & S.print
  & runResourceT -- >>= pure . S.fst'

sumNewCases :: CountryData -> Int
sumNewCases cd = sum $ cd ^. days ^.. folded . _2 . cases . new_cases

toCountryData :: (Monad m, MonadResource m) =>
   Stream (Of (BSC.ByteString, BSC.ByteString)) m x
   -> m (Of (Maybe CountryData) x)
toCountryData str = S.next str >>= withCountryData
  where
    withCountryData (Left r) = pure $ Nothing S.:> r
    withCountryData (Right (line1, str')) =
      S.fold (flip addDay) (initCD line1) id str'

    initCD (code, rest) = A.maybeResult $ A.parse (fullCountryData code) rest
    addDay (_, r) = _Just . days %~ (parseDayInfo r ++)
    parseDayInfo r = fromRight [] $ A.parseOnly dayInfoOnly r

main :: IO ()
main = do
  readCovidData >>= print
