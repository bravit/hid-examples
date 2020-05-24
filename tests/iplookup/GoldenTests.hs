module GoldenTests (goldenTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath (normalise, takeBaseName, replaceExtension)

import ParseIP
import LookupIP

goldenTests :: IO [TestTree]
goldenTests = sequence [golden_lookupIP]

testsDir :: FilePath
testsDir = normalise "data/tests/iplookup/"

golden_lookupIP :: IO TestTree
golden_lookupIP = testGroup "lookupIP" . map createTest
                  <$> findByExtension [".iprs"] testsDir

createTest :: String -> TestTree
createTest iprsf = goldenVsFile
                    (takeBaseName iprsf)
                    goldenf
                    outf
                    testAction
  where
    ipsf = replaceExtension iprsf ".ips"
    goldenf = replaceExtension iprsf ".out.golden"
    outf = replaceExtension iprsf ".out"
    testAction = do
      iprs <- parseValidIPRanges <$> readFile iprsf
      ips <- parseValidIPs <$> readFile ipsf
      writeBinaryFile outf $ reportIPs iprs ips
