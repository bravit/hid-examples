module GoldenTests where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath

import ParseIP
import ReportIP

golden_tests :: IO TestTree
golden_tests = do
    iprsFiles <- findByExtension [".iprs"] dir
    pure $ testGroup "lookupIP golden tests" (map createTest iprsFiles)
  where
    dir = "data/tests/iplookup/"
    createTest iprsF =
      let name = takeBaseName iprsF
          withpath = dir ++ name
      in goldenVsFile name
                      (withpath ++ ".out.golden")
                      (withpath ++ ".out")
                      (doTest (withpath ++ ".iprs")
                              (withpath ++ ".ips")
                              (withpath ++ ".out"))
    doTest fp1 fp2 fp3 = do
      iprdb <- parseValidIPRanges <$> readFile fp1
      ips <- parseValidIPs <$> readFile fp2
      reportIPsToFile iprdb ips fp3
