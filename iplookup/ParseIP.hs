{-# LANGUAGE TypeApplications #-}

module ParseIP where

import Data.Word
import Data.Bits (shiftL, toIntegralSized)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
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

-- | Checks if the list has the given length
--
-- >>> 4 `isLengthOf` [1,2,3,4]
-- True
-- >>> 0 `isLengthOf` []
-- True
-- >>> 0 `isLengthOf` [1,2,3,4]
-- False

isLengthOf :: Int -> [a] -> Bool
isLengthOf n xs = length xs == n

-- | Parses IP address given as a 'String'
--
-- >>> parseIP "0.0.0.0"
-- Just 0.0.0.0
--
-- >>> parseIP "192.168.3.15"
-- Just 192.168.3.15
--
-- >>> parseIP "not an IP address"
-- Nothing

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

parseValidIPs :: String -> [IP]
parseValidIPs = mapMaybe parseIP . lines

parseValidIPRanges :: String -> IPRangeDB
parseValidIPRanges = IPRangeDB . mapMaybe parseIPRange . lines
