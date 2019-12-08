module Main where

import IPTypes
import ParseIP
import LookupIP
import FastLookup
import System.Random
import Data.Word

ipdb = "data/ipdb.txt"
nReqs = 500000

genIPList :: Int -> [IP]
genIPList n = map IP $ take n $ iterate (+step) 0
  where
    step = maxBound `div` fromIntegral n

simulate :: IPRangeDB -> [IP] -> (Int, Int)
simulate iprdb ips = (yes, no)
  where
    imap = ipRangeDB2IntervalMap iprdb
    yes = length $ filter id $ map (fastLookupIP imap) ips
    no = nReqs - yes
{-  
    yes = {-# SCC yes #-} length $ filter id $ map (lookupIP iprdb) ips
    no = {-# SCC no #-} nReqs {- length ips -} -- yes
-}

report :: (Int, Int) -> IO ()
report info = putStrLn $ "(yes, no) = " ++ show info

main = do
  iprs <- parseIPRanges <$> readFile ipdb
  let ips = genIPList nReqs
  case iprs of
    Right iprs -> report (simulate iprs ips)
    _ -> print "Can't read IP ranges database"