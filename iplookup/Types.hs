module Types where

import Data.Word
import Control.Exception.Safe

type IP = Word32

type IPRange = (IP, IP)

type IPRangeDB = [IPRange]

type LineNumber = Int

newtype ParseError = ParseError LineNumber
  deriving (Show, Eq)

data InvalidArgsException = LoadIPRangesError ParseError
                          | InvalidIP String

instance Show InvalidArgsException where
  show (LoadIPRangesError (ParseError idx)) =
    "Error loading ip range databases (line: " ++ show idx ++ ")"
  show (InvalidIP s) = "Invalid IP address to check: " ++ s

instance Exception InvalidArgsException
