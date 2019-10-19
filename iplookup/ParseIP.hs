{-# LANGUAGE TypeApplications #-}

module ParseIP where

import Data.Char
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

parseIP' :: String -> Maybe IP
parseIP' cs
  | null strIPComponents ||
    i1<0 || i2<0 || i3<0 || i4<0 = Nothing
  | otherwise = Just $ IP $ fromIntegral $
                i4 + shiftL8 (i3 + shiftL8 (i2 + shiftL8 i1))
  where
    shiftL8 a = shiftL a 8
    [i1, i2, i3, i4] = map ipComponentToInt strIPComponents
    ipComponentToInt cs =
      case map digitToInt cs of
        [n] -> n
        [n1, n2] -> n1 * 10 + n2
        [n1, n2, n3] -> let n = n1 * 100 + n2 * 10 + n3
                        in if n <=255 then n
                                      else -1
        _ -> -1
    strIPComponents =
      case span isDigit cs of
        (p1, '.':rest) ->
          case span isDigit rest of
            (p2, '.':rest) ->
              case span isDigit rest of
                (p3, '.':p4) ->
                  if all isDigit p4 then [p1,p2,p3,p4] else []
                _ -> []
            _ -> []
        _ -> []

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
