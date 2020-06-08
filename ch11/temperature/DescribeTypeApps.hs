{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DescribeTypeApps where

import TempPhantom

class Describe a where
  describe :: String

instance Describe F where
  describe = "Fahrenheit degrees"

instance Describe C where
  describe = "Celsius degrees"

instance Describe Temp where
  describe = "Temperature type constructor"

instance Describe u => Describe (Temp u) where
  describe = "Temperature in " ++ describe @u

answer1, answer2, answer3 :: String

answer1 = describe @F
answer2 = describe @(Temp C)
answer3 = describe @Temp

testDescribeTypeApps :: IO ()
testDescribeTypeApps = do
  putStrLn answer1
  putStrLn answer2
  putStrLn answer3
