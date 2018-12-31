module ParseIP where

import Data.Word
import Data.Bits (shiftL)
import Data.List.Split
import Control.Applicative
import Control.Monad
import Safe

import Types

buildIP :: [Word32] -> IP
buildIP = fst . foldr go (0, 1)
  where
    go b (s, k) = (s+b*k, k*256)

buildIP' :: [Word32] -> IP
buildIP' = foldl1 (\s b -> s*256 + b)

buildIP'' :: [Word32] -> IP
buildIP'' = foldl1 (\s b -> shiftL s 8 + b)

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded f a = if f a then pure a else empty

isLengthOf :: Int -> [a] -> Bool 
isLengthOf n xs = length xs == n

parseIP :: String -> Maybe IP
parseIP = guarded (4 `isLengthOf`) . splitOn "."
          >=> mapM (readMay >=> guarded fitsOctet >=> pure . fromInteger)
          >=> pure . buildIP
  where
    fitsOctet x = 0 <= x && x < 256

parseIPRange :: String -> Maybe IPRange
parseIPRange = guarded (2 `isLengthOf`) . splitOn ","
               >=> mapM parseIP
               >=> listToPair
  where
    listToPair [a,b] = pure (a,b)
    listToPair _ = empty

parseIPRanges :: String -> Either ParseError IPRangeDB
parseIPRanges = mapM parseLine . zip [1..] . lines
  where
    parseLine (ln, s) = case parseIPRange s of
                           Nothing -> Left (ParseError ln)
                           Just ipr -> Right ipr
