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

type CountryCodeWithRest = (BSC.ByteString, BSC.ByteString)

readCovidCSV :: Monad m =>
   C.ByteString m r
   -> Stream (Stream (Of CountryCodeWithRest) m) m ()
readCovidCSV str =
  ABS.parsed countryCodeWithRest str
  & void
  & S.drop 1
  & S.filter (isCountry . fst)
  & S.groupBy ((==) `on` fst)
  where
    isCountry bs = BSC.length bs == 3

parseCountryData :: Monad m =>
   Stream (Stream (Of CountryCodeWithRest) m) m x
   -> Stream (Of CountryData) m x
parseCountryData str =
  S.mapped tryMkCountryData str
  & S.catMaybes
  & S.map (days %~ reverse)

tryMkCountryData :: Monad m =>
      Stream (Of CountryCodeWithRest) m x ->
      m (Of (Maybe CountryData) x)
tryMkCountryData str = S.next str >>= either noCountryData withCountryData
  where
    withCountryData (line1, rest) =
      case initCD line1 of
        Nothing -> S.effects rest >>= noCountryData
        Just cd -> first Just <$> S.fold (flip addDay) cd id rest

    noCountryData = pure . (Nothing S.:>)

    initCD (code, rest) = A.maybeResult $ A.parse (fullCountryData code) rest
    addDay (_, r) = days %~ (parseDayInfo r ++)
    parseDayInfo r = fromRight [] $ A.parseOnly dayInfoOnly r

processCountryData :: (Monad m, MonadIO m) =>
  Stream (Of CountryData) m () -> m ()
processCountryData str =
  S.store (S.sum . S.map sumNewCases) str
  & S.store (S.sum . S.map pop)
  & S.print >>= liftIO . print

pop :: CountryData -> Int
pop cd = cd ^. stat . population

sumNewCases :: CountryData -> Int
sumNewCases cd = sum $ cd ^. days ^.. folded . _2 . cases . new_cases



-- https://ourworldindata.org/coronavirus-source-data


main :: IO ()
main =
  C.readFile "data/owid-covid-data.csv.gz"
  & gunzip
  & readCovidCSV
  & parseCountryData
  & processCountryData
  & runResourceT

