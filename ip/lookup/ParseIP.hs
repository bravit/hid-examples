{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module ParseIP where

import Data.Char
import Data.Word
import Data.Bits (shiftL, toIntegralSized)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Control.Applicative
import Control.Monad
import Text.Read

import IPTypes

buildIP :: [Word8] -> IP
buildIP = buildIP_foldl

{-# INLINE buildIP_foldr #-}
buildIP_foldr :: [Word8] -> IP
buildIP_foldr = IP . fst . foldr go (0, 1)
  where
    go b (s, k) = (s + fromIntegral b * k, k*256)

{-# INLINE buildIP_foldl #-}
buildIP_foldl :: [Word8] -> IP
buildIP_foldl = IP . foldl (\s b -> s*256 + fromIntegral b) 0

{-# INLINE buildIP_foldl_shl #-}
buildIP_foldl_shl :: [Word8] -> IP
buildIP_foldl_shl = IP . foldl (\s b -> shiftL s 8 + fromIntegral b) 0

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

-- | Parses the IP address given as a 'String'
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
parseIP = parseIPIterStrict

{-# INLINE parseIPMonadic #-}
parseIPMonadic :: String -> Maybe IP
parseIPMonadic = guarded (4 `isLengthOf`) . splitOn "."
          >=> mapM (readMaybe @Integer >=> toIntegralSized)
          >=> pure . buildIP

{-# INLINE parseIPIter #-}
parseIPIter :: String -> Maybe IP
parseIPIter chars = go chars 0 0 1 0
  where
    go :: String -> Int -> Int -> Int -> Int -> Maybe IP
    go (c:cs) ip ipcomp ncomp ndigit
      | isDigit c && ndigit < 3
        = go cs ip (addDigit ipcomp c) ncomp (ndigit + 1)
      | c == '.'  && ncomp < 4 && goodComp ndigit ipcomp
        = go cs (addComp ip ipcomp) 0 (ncomp + 1) 0
    go [] ip ipcomp ncomp ndigit
      | ncomp == 4 && goodComp ndigit ipcomp
        = Just $ IP $ fromIntegral $ addComp ip ipcomp
    go _ _ _ _ _ = Nothing

    goodComp 1 _ = True
    goodComp 2 _ = True
    goodComp 3 ipcomp = ipcomp <= 255
    goodComp _ _ = False

    addComp ip ipcomp = shiftL ip 8 + ipcomp
    addDigit ipcomp c = ipcomp * 10 + digitToInt c

{-# INLINE parseIPIterStrict #-}
parseIPIterStrict :: String -> Maybe IP
parseIPIterStrict chars = go chars 0 0 1 0
  where
    go :: String -> Int -> Int -> Int -> Int -> Maybe IP
    go (c:cs) !ip !ipcomp !ncomp !ndigit
      | isDigit c && ndigit < 3
        = go cs ip (addDigit ipcomp c) ncomp (ndigit + 1)
      | c == '.'  && ncomp < 4 && goodComp ndigit ipcomp
        = go cs (addComp ip ipcomp) 0 (ncomp + 1) 0
    go [] !ip !ipcomp !ncomp !ndigit
      | ncomp == 4 && goodComp ndigit ipcomp
        = Just $ IP $ fromIntegral $ addComp ip ipcomp
    go _ _ _ _ _ = Nothing

    goodComp 1 _ = True
    goodComp 2 _ = True
    goodComp 3 !ipcomp = ipcomp <= 255
    goodComp _ _ = False

    addComp !ip !ipcomp = shiftL ip 8 + ipcomp
    addDigit !ipcomp !c = ipcomp * 10 + digitToInt c


parseIP'' :: String -> Maybe IP
parseIP'' cs
  | null strIPComponents ||
    i1<0 || i2<0 || i3<0 || i4<0 = Nothing
  | otherwise = Just $ IP $ fromIntegral $
                i4 + shiftL8 (i3 + shiftL8 (i2 + shiftL8 i1))
  where
    shiftL8 a = shiftL a 8
    [i1, i2, i3, i4] = map ipComponentToInt strIPComponents
    ipComponentToInt ipc =
      case map digitToInt ipc of
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
            (p2, '.':rest') ->
              case span isDigit rest' of
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
