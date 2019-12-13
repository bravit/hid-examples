module Main where

import IPTypes
import ParseIP
import FastLookup
import System.Random
import Data.Word

ipdb = "data/ipdb.txt"
nReqs = 500000

genIPList :: Int -> [IP]
genIPList n = map IP $ take n $ iterate (+step) 0
  where
    step = maxBound `div` fromIntegral n

simulate :: FastIPRangeDB -> [IP] -> (Int, Int)
simulate iprdb ips = (yes, no)
  where
    yes = length $ filter id $ map (lookupIP iprdb) ips
    no = nReqs - yes

report :: (Int, Int) -> IO ()
report info = putStrLn $ "(yes, no) = " ++ show info

main = do
  iprs <- parseIPRanges <$> readFile ipdb
  let ips = genIPList nReqs
  case iprs of
    Right iprs -> report $ simulate (fromIPRangeDB iprs) ips
    _ -> print "Can't read IP ranges database"