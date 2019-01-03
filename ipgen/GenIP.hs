module GenIP where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Word
import Data.List
import Control.Monad

import Types

showIP :: IP -> String
showIP ip = concat $ intersperse "." $ map show [b4,b3,b2,b1]
  where
    (ip1, b1) = ip `divMod` 256
    (ip2, b2) = ip1 `divMod` 256
    (b4, b3) = ip2 `divMod` 256

showIPRange :: IPRange -> String
showIPRange (ip1, ip2) = showIP ip1 ++ "," ++ showIP ip2

showIPRangeDB :: IPRangeDB -> String
showIPRangeDB = unlines . map showIPRange

genIP :: Gen IP
genIP = Gen.word32 Range.linearBounded

genIPComponents :: Gen [Word32]
genIPComponents = Gen.list (Range.singleton 4) genOctet
  where genOctet = Gen.word32 (Range.linear 0 255)

genIPString :: Gen String
genIPString = concat . intersperse "." . map show <$> genIPComponents 

genIPWithString :: Gen (IP, String)
genIPWithString = do
  ip <- genIP
  pure (ip, showIP ip)

genIPRange :: Gen IPRange
genIPRange = do
  ip1 <- genIP
  ip2 <- Gen.word32 (Range.linearFrom (ip1 + 1) ip1 maxBound)
  pure (ip1, ip2)

genInvalidIPRange :: Gen IPRange
genInvalidIPRange = do
  ip1 <- genIP
  ip2 <- Gen.word32 (Range.linear minBound (ip1 - 1))
  pure (ip1, ip2)

genIPRangeString :: Gen String
genIPRangeString = showIPRange <$> genIPRange

genIPRangeWithString :: Gen (IPRange, String)
genIPRangeWithString = do
  ipr <- genIPRange
  pure (ipr, showIPRange ipr)

genInvalidIPRangeWithString :: Gen (IPRange, String)
genInvalidIPRangeWithString = do
  ipr <- genInvalidIPRange
  pure (ipr, showIPRange ipr)

genIPRangeDB :: Gen IPRangeDB
genIPRangeDB = Gen.list (Range.exponential 1 100) genIPRange

genIPRangeDBWithString :: Gen (IPRangeDB, String)
genIPRangeDBWithString = do
  iprdb <- genIPRangeDB
  pure (iprdb, showIPRangeDB iprdb)

genIPRangesString :: Int -> Gen String
genIPRangesString n = unlines <$> Gen.list (Range.singleton n) genIPRangeString
