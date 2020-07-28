module GenIP where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Word
import Data.List (intercalate)

import IPTypes

genIP :: Gen IP
genIP = IP <$> Gen.word32 Range.linearBounded

genIPComponents :: Gen [Word8]
genIPComponents = Gen.list (Range.singleton 4) genOctet
  where genOctet = Gen.word8 Range.linearBounded

genIPString :: Gen String
genIPString = intercalate "." . map show <$> genIPComponents

genIPRange :: Gen IPRange
genIPRange = do
  (IP ip1) <- genIP
  ip2 <- Gen.word32 (Range.linearFrom (ip1 + 1) ip1 (ip1 + mb ip1))
  pure $ IPRange (IP ip1) (IP ip2)
  where
    maxRangeSize = 1000000
    mb from = min (maxBound - from) maxRangeSize

genInvalidIPRange :: Gen IPRange
genInvalidIPRange = do
  (IP ip1) <- Gen.filter (> IP minBound) genIP
  ip2 <- Gen.word32 (Range.linear minBound (ip1 - 1))
  pure $ IPRange (IP ip1) (IP ip2)

genIPRangeDBSized :: Int -> Int -> Gen IPRangeDB
genIPRangeDBSized minLen maxLen = IPRangeDB <$> Gen.list (Range.constant minLen maxLen) genIPRange

genIPRangeDB :: Gen IPRangeDB
genIPRangeDB = do
  n1 <- Gen.integral (Range.constant 1 100)
  n2 <- Gen.integral (Range.constant n1 100)
  genIPRangeDBSized n1 n2

genIPList :: Int -> IO [IP]
genIPList n = Gen.sample $ Gen.list (Range.constant n n) genIP
