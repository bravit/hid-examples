module ParseIPSpec where

import Data.Maybe
import Data.Word
import Test.Tasty.Hspec

import ParseIP

parseIPSpecs :: Spec
parseIPSpecs = describe "ParseIP" $ do
  spec_buildIP
  spec_parseIP
  spec_parseIPRange
  spec_parseIPRanges

spec_buildIP :: Spec
spec_buildIP =
  describe "buildIP" $ do
    it "builds from zero" $
      buildIP [0,0,0,0] `shouldBe` 0
    it "builds from one" $
      buildIP [0,0,0,1] `shouldBe` 1
    it "builds from localhost" $
      buildIP [127,0,0,1] `shouldBe` 1 + 127 * 256^3
    it "builds from arbitrary address" $
      buildIP [192,168,3,15] `shouldBe`
        15 + 3 * 256 + 168 * 256^2 + 192 * 256^3

spec_parseIP :: Spec
spec_parseIP =
  describe "parseIP" $ do
    it "parses zero" $
      parseIP "0.0.0.0" `shouldBe` Just 0
    it "parses one" $
      parseIP "0.0.0.1" `shouldBe` Just 1
    it "parses the largest IP address" $
      parseIP "255.255.255.255" `shouldBe` Just maxBound
    it "parses some random IP address" $
      parseIP "192.168.3.15" `shouldBe`
        Just (buildIP [192,168,3,15])
    it "fails to parse 3 components" $
      parseIP "192.168.1" `shouldBe` Nothing
    it "fails to parse 4 components with suffix" $
      parseIP "192.168.1.1x" `shouldBe` Nothing
    it "fails to parse empty component" $ 
      parseIP "192.168..1" `shouldBe` Nothing
    it "fails to parse 5 components" $
      parseIP "192.168.1.0.1" `shouldBe` Nothing
    it "fails to parse large components" $
      parseIP "256.168.1.0" `shouldBe` Nothing
    it "fails to parse extremely large components" $
      parseIP "0.0.0.4294967338" `shouldBe` Nothing
    it "fails to parse negative components" $
      parseIP "256.168.-1.0" `shouldBe` Nothing
    it "fails to parse non-numeric components" $
      parseIP "192.x.1.0" `shouldBe` Nothing

spec_parseIPRange :: Spec
spec_parseIPRange =
  describe "parseIPRange" $ do
    it "parses pair" $
      parseIPRange "192.168.0.1,192.168.3.100" `shouldBe`
        (,) <$> parseIP "192.168.0.1" <*> parseIP "192.168.3.100"
    it "fails to parse single ip" $
      parseIPRange "192.168.0.1" `shouldBe` Nothing
    it "fails to parse single ip with comma" $
      parseIPRange "192.168.0.1," `shouldBe` Nothing
    it "fails to parse triple" $
      parseIPRange "192.168.0.1,192.168.0.2,192.168.0.4" `shouldBe` Nothing

spec_parseIPRanges :: Spec
spec_parseIPRanges =
  describe "parseIPRanges" $ do
    let sample_range = "192.168.0.1,192.168.3.100"
        sample_range2 = "0.0.0.0,0.0.0.1"
    it "parses empty list" $
      parseIPRanges ""  `shouldBe` Right []
    it "parses single range" $
      parseIPRanges sample_range  `shouldBe`
        Right [fromJust $ parseIPRange sample_range]
    it "parses two ranges" $
      parseIPRanges (sample_range ++ "\n" ++ sample_range2) `shouldBe`
        Right [fromJust $ parseIPRange sample_range,
               fromJust $ parseIPRange sample_range2]
