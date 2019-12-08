module Main where

import IPTypes
import ParseIP
import LookupIP
import System.Random

ipdb = "data/ipdb.txt"
nReqs = 500000

simulate :: IPRangeDB -> [IP] -> (Int, Int)
simulate iprdb ips = (yes, no)
  where
    yes = {-# SCC yes #-} length $ filter id $ map (lookupIP iprdb) ips
    no = {-# SCC no #-} length ips - yes

genIPList :: Int -> [IP]
genIPList n = map IP $ take n $ iterate (+step) 0
  where
    step = fromIntegral $ 2^32 `div` n

report :: (Int, Int) -> IO ()
report info = putStrLn $ "(yes, no) = " ++ show info

main = do
  iprs <- parseIPRanges <$> readFile ipdb
  let ips = genIPList nReqs
  case iprs of
    Right iprs -> report (simulate iprs ips)
    _ -> print "Can't read IP ranges database"