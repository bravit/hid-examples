module GoldenTests where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath (replaceExtension, takeBaseName)
import Data.Foldable (fold)

import ParseIP
import ReportIP

golden_tests :: IO [TestTree]
golden_tests = sequence [golden_lookupIP]

golden_lookupIP :: IO TestTree
golden_lookupIP = do
    iprsFiles <- findByExtension [".iprs"] dir
    pure $ testGroup "lookupIP golden tests" (map createTest iprsFiles)
  where
    dir = "data/tests/iplookup/"
    createTest iprsF =
      goldenVsFile (takeBaseName iprsF)
                   (replaceExtension iprsF ".out.golden")
                   (replaceExtension iprsF ".out")
                   $ do
        iprdb <- parseValidIPRanges <$> readFile iprsF
        ips <- parseValidIPs <$> readFile (replaceExtension iprsF ".ips")
        writeBinaryFile (replaceExtension iprsF ".out") $ reportIPs iprdb ips
