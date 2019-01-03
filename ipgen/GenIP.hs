module GenIP where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Word
import Data.List
import Control.Monad

import IPTypes

genIP :: Gen IP
genIP = IP <$> Gen.word32 Range.linearBounded

genIPComponents :: Gen [Word8]
genIPComponents = Gen.list (Range.singleton 4) genOctet
  where genOctet = Gen.word8 Range.linearBounded

genIPString :: Gen String
genIPString = concat . intersperse "." . map show <$> genIPComponents 

genIPRange :: Gen IPRange
genIPRange = do
  (IP ip1) <- genIP
  ip2 <- Gen.word32 (Range.linearFrom (ip1 + 1) ip1 maxBound)
  pure $ IPRange (IP ip1) (IP ip2)

genInvalidIPRange :: Gen IPRange
genInvalidIPRange = do
  (IP ip1) <- genIP
  ip2 <- Gen.word32 (Range.linear minBound (ip1 - 1))
  pure $ IPRange (IP ip1) (IP ip2)

genIPRangeDBSized :: Int -> Int -> Gen IPRangeDB
genIPRangeDBSized minLen maxLen = IPRangeDB <$> Gen.list (Range.constant minLen maxLen) genIPRange

genIPRangeDB :: Gen IPRangeDB
genIPRangeDB = do
  n1 <- Gen.integral (Range.constant 1 100)
  n2 <- Gen.integral (Range.constant n1 100)
  genIPRangeDBSized n1 n2
