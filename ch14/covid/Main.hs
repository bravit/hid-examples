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
import Data.Text (Text)
import qualified Data.Text.IO as T
import TextShow
import Data.Map (Map)
import qualified Data.Map as M

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
  & S.map (\cd -> cd & current_total_cases .~ currentTotalCases cd)
  & S.map (\cd -> cd & current_total_deaths .~ currentTotalDeaths cd)
  where
    currentTotalCases cd =
      maximum $ cd ^. days ^.. folded . _2 . cases . total_cases

    currentTotalDeaths cd =
      maximum $ cd ^. days ^.. folded . _2 . deaths . total_deaths

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

byContinents :: (Monad m, MonadIO m) =>
  Stream (Of CountryData) m r -> m (Of (Map Text AccumulatedStat) r)
byContinents = S.fold enrich M.empty id
  where
    enrich stats cd = M.alter (fromCD cd <>) (cd ^. continent) stats

    fromCD cd =
      Just $ AccumulatedStat (cd ^. stat . population)
                             (cd ^. current_total_cases)
                             (cd ^. current_total_deaths)

printCountryData :: (MonadIO m, TextShow a) => Stream (Of a) m r -> m r
printCountryData str = do
  liftIO $ T.putStrLn "Country population cases deaths"
  S.mapM_ (liftIO . printT) str

printContinentStats :: Map Text AccumulatedStat -> IO ()
printContinentStats stats = do
  T.putStrLn "\nContinent/population/cases/deaths"
  forM_ (M.toAscList stats) $ \(nm, st) -> do
    T.putStr $ nm <> "/"
    printT st
  T.putStrLn $ "\nWorld population/cases/deaths: "
  printT $ M.foldl' (<>) mempty stats

-- https://ourworldindata.org/coronavirus-source-data

main :: IO ()
main = do
  r <- C.readFile "data/owid-covid-data.csv.gz"
       & gunzip
       & readCovidCSV
       & parseCountryData
       & S.store byContinents
       & printCountryData
       & runResourceT
  printContinentStats $ S.fst' r

