{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Function (on, (&))

import Streaming
import Streaming.Zip
import qualified Streaming.Prelude as S
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Streaming.Char8 as C
import qualified Data.Attoparsec.ByteString.Streaming as ABS
import Data.Text (Text)
import qualified Data.Text.IO as T
import TextShow
import Data.Map (Map)
import qualified Data.Map as M

import CovidData
import CovidCSVParser

tryMkCountryData :: Monad m =>
      Stream (Of CountryCodeWithRest) m r ->
      m (Of (Maybe CountryData) r)
tryMkCountryData str = S.next str >>= either noCountryData withCountryData
  where
    withCountryData (line1, otherLines) =
      case parseFullCountryData line1 of
        Nothing -> S.effects otherLines >>= noCountryData
        Just cd -> first (Just . addDays cd)
                   <$> (S.mconcat $ S.map parseDayInfo otherLines)

    noCountryData = pure . (Nothing :>)

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

main :: IO ()
main = do
-- Data source: https://ourworldindata.org/coronavirus-source-data
  r <- C.readFile "data/owid-covid-data.csv.gz"
       & gunzip
       & ABS.parsed countryCodeWithRestOrSkip
       & void
       & S.catMaybes
       & S.groupBy ((==) `on` code)
       & mapsM tryMkCountryData
       & S.catMaybes
       & S.store (S.fold considerCountry M.empty id)
       & printCountryData
       & runResourceT
  printStats $ S.fst' r
