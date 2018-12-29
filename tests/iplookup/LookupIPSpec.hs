module LookupIPSpec where

import Test.Tasty.Hspec
import Data.Word

import LookupIP

lookupIPSpecs :: Spec
lookupIPSpecs = describe "LookupIPSpec" $ do
  spec_lookupIP

spec_lookupIP :: Spec
spec_lookupIP = do
  let sample_iprdb = [(0,1), (100, 120)]
  describe "lookupIP" $ do
    it "no IP in empty list" $ do
      0 `shouldNotSatisfy` lookupIP [] 
    it "IP in sample list" $ do
      110 `shouldSatisfy` lookupIP sample_iprdb 
    it "no IP in sample list" $ do
      50 `shouldNotSatisfy` lookupIP sample_iprdb 
