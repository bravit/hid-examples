module LookupIPSpec where

import Test.Tasty.Hspec

import IPTypes
import LookupIP

lookupIPSpecs :: Spec
lookupIPSpecs = describe "LookupIP" $ do
  spec_lookupIP

spec_lookupIP :: Spec
spec_lookupIP =
  describe "lookupIP" $ do
    let empty_iprdb = IPRangeDB []
        sample_iprdb = IPRangeDB [IPRange (IP 0) (IP 1), IPRange (IP 100) (IP 120)]
        ip1 = IP 110
        ip2 = IP 50
    it "no IP in empty list" $
      ip1 `shouldNotSatisfy` lookupIP empty_iprdb
    it "IP in sample list" $
      ip1 `shouldSatisfy` lookupIP sample_iprdb
    it "no IP in sample list" $
      ip2 `shouldNotSatisfy` lookupIP sample_iprdb
