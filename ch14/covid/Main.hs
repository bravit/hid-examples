{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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

readCovidCSV :: Monad m => C.ByteString m r -> Stream (Of CountryData) m ()
readCovidCSV str =
  ABS.parsed countryCodeWithRest str
  & void
  & S.filter (isCountry . fst)
  & S.groupBy ((==) `on` fst)
  & S.mapped tryMkCountryData
  & S.catMaybes
  & S.map (\cd -> cd & current_total_deaths .~ currentTotalDeaths cd)
  & S.map (currentTotalCases >>= set current_total_cases)
  where
    isCountry bs = BSC.length bs == 3

    currentTotalDeaths cd =
      maximum $ cd ^. days ^.. folded . _2 . deaths . total_deaths

    currentTotalCases =
      maximum1Of (folded . _2 . cases . total_cases) . view days

tryMkCountryData :: Monad m =>
      Stream (Of CountryCodeWithRest) m r ->
      m (Of (Maybe CountryData) r)
tryMkCountryData str = S.next str >>= either noCountryData withCountryData
  where
    withCountryData (line1, rest) =
      case initCD line1 of
        Nothing -> S.effects rest >>= noCountryData
        Just cd -> first (Just . addDays cd) <$> parseRest rest

    noCountryData = pure . (Nothing S.:>)

    parseRest rest = S.mconcat $ S.map (parseDayInfo . snd) rest

    initCD (code, rest) = A.maybeResult $ A.parse (fullCountryData code) rest
    parseDayInfo r = fromRight [] $ A.parseOnly dayInfoOnly r

    addDays cd ds = cd & days %~ (++ ds)

byContinents :: (Monad m, MonadIO m) =>
  Stream (Of CountryData) m r -> m (Of (Map Text AccumulatedStat) r)
byContinents = S.fold enrich M.empty id
  where
    enrich stats cd =
      M.insertWith (<>) (cd ^. continent) (fromCountryData cd) stats
--    enrich stats cd = stats &
--      let new = fromCountryData cd
--      in at (cd ^. continent) %~ Just . maybe new (<> new)

printCountryData :: (MonadIO m, TextShow a) => Stream (Of a) m r -> m r
printCountryData str = do
  liftIO $ T.putStrLn "Country population cases deaths"
  S.mapM_ (liftIO . printT) str

printStats :: Map Text AccumulatedStat -> IO ()
printStats stats = do
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
       & S.store byContinents
       & printCountryData
       & runResourceT
  printStats $ S.fst' r
