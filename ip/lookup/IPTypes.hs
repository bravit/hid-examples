{-# LANGUAGE DeriveAnyClass #-}

module IPTypes where

import Data.Word
import Data.List (intercalate)
import Control.Exception (Exception)

newtype IP = IP {unIP :: Word32}
  deriving (Eq, Ord)

instance Show IP where
  show (IP ip) = intercalate "." $ map show [b4,b3,b2,b1]
    where
      (ip1, b1) = ip `divMod` 256
      (ip2, b2) = ip1 `divMod` 256
      (b4, b3) = ip2 `divMod` 256

data IPRange = IPRange IP IP
  deriving Eq

instance Show IPRange where
  show (IPRange ip1 ip2) = show ip1 ++ "," ++ show ip2

data IPRangeDB = IPRangeDB [IPRange]
  deriving Eq

instance Show IPRangeDB where
  show (IPRangeDB iprs) = unlines $ map show iprs

type LineNumber = Int

newtype ParseError = ParseError LineNumber
  deriving (Show, Eq)

data InvalidArgsException = LoadIPRangesError ParseError
                          | InvalidIP String
  deriving Exception

instance Show InvalidArgsException where
  show (LoadIPRangesError (ParseError idx)) =
    "Error loading ip range databases (line: " ++ show idx ++ ")"
  show (InvalidIP s) = "Invalid IP address to check: " ++ s
