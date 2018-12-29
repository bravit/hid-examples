module LookupIPSpec where

import Test.Tasty.Hspec
import Data.Word

import LookupIP

lookupIPSpecs :: Spec
lookupIPSpecs = describe "LookupIPSpec" $ do
  spec_lookupIP

spec_lookupIP :: Spec
spec_lookupIP = do
  let empty_iprdb = []
      sample_iprdb = [(0,1), (100, 120)]
      ip1 = 110
      ip2 = 50
  describe "lookupIP" $ do
    it "no IP in empty list" $ do
      ip1 `shouldNotSatisfy` lookupIP empty_iprdb
    it "IP in sample list" $ do
      ip1 `shouldSatisfy` lookupIP sample_iprdb 
    it "no IP in sample list" $ do
      ip2 `shouldNotSatisfy` lookupIP sample_iprdb 
