{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DescribeProxy where

import Data.Proxy
import TempPhantom

-- data Proxy t = Proxy

class DescribeType a where
  describe :: Proxy a -> String

instance DescribeType F where
  describe :: Proxy F -> String
  describe _ = "Fahrenheit degrees"

instance DescribeType C where
  describe :: Proxy C -> String
  describe _ = "Celsius degrees"

instance DescribeType u => DescribeType (Temp u) where
  describe :: Proxy (Temp u) -> String
  describe _ = "Temperature in " ++ describe (Proxy :: Proxy u)

-- We need the PolyKinds extension to define this instance
instance DescribeType Temp where
  describe _ = "Temperature type constructor"

-- class JSONSchema a where
--   schema :: Proxy a -> Schema

someFunc :: proxy a -> String
someFunc _ = "OK"

testDescribeProxy :: IO ()
testDescribeProxy = do
  putStrLn $ describe (Proxy :: Proxy C)
  putStrLn $ describe (Proxy :: Proxy F)
  putStrLn $ describe (Proxy :: Proxy (Temp C))
  putStrLn $ describe (Proxy :: Proxy (Temp F))
  putStrLn $ describe (Proxy :: Proxy Temp)
  putStrLn $ someFunc (Proxy :: Proxy Bool)
