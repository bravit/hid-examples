{-# LANGUAGE TypeApplications #-}

module ParseIP where

import Data.Word
import Data.Bits (shiftL, toIntegralSized)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Control.Applicative
import Control.Monad
import Safe

import IPTypes

buildIP :: [Word8] -> IP
buildIP = IP . fst . foldr go (0, 1)
  where
    go b (s, k) = (s + fromIntegral b * k, k*256)

buildIP' :: [Word8] -> IP
buildIP' = IP . foldl (\s b -> s*256 + fromIntegral b) 0

buildIP'' :: [Word8] -> IP
buildIP'' = IP . foldl (\s b -> shiftL s 8 + fromIntegral b) 0

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded f a = if f a then pure a else empty

isLengthOf :: Int -> [a] -> Bool 
isLengthOf n xs = length xs == n

parseIP :: String -> Maybe IP
parseIP = guarded (4 `isLengthOf`) . splitOn "."
          >=> mapM (readMay @Integer >=> toIntegralSized)
          >=> pure . buildIP

parseIPRange :: String -> Maybe IPRange
parseIPRange = guarded (2 `isLengthOf`) . splitOn ","
               >=> mapM parseIP
               >=> listToIPRange
  where
    listToIPRange [a,b]
      | a <= b = pure (IPRange a b)
    listToIPRange _ = empty

parseIPRanges :: String -> Either ParseError IPRangeDB
parseIPRanges = fmap IPRangeDB . mapM parseLine . zip [1..] . lines
  where
    parseLine (ln, s) = case parseIPRange s of
                           Nothing -> Left (ParseError ln)
                           Just ipr -> Right ipr

parseValidIPRanges :: String -> IPRangeDB
parseValidIPRanges = IPRangeDB . catMaybes . map parseIPRange . lines

parseValidIPs :: String -> [IP]
parseValidIPs = catMaybes . map parseIP . lines
