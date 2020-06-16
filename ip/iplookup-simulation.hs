module Main where

import IPTypes
import ParseIP
--import LookupIP
import FastLookup

ipdb :: String
ipdb = "data/ipdb.txt"

nReqs :: Int
nReqs = 500000

genIPList :: Int -> [IP]
genIPList n = map IP $ take n $ iterate (+step) 0
  where
    step = maxBound `div` fromIntegral n

--simulate :: IPRangeDB -> [IP] -> (Int, Int)
simulate :: FastIPRangeDB -> [IP] -> (Int, Int)
simulate iprdb ips = (yes, no)
  where
    yes = length $ filter id $ map (lookupIP iprdb) ips
--    no = length ips - yes
    no = nReqs - yes

report :: (Int, Int) -> IO ()
report info = putStrLn $ "(yes, no) = " ++ show info

main :: IO ()
main = do
  iprs <- parseIPRanges <$> readFile ipdb
  let ips = genIPList nReqs
  case iprs of
    Right iprs' -> report $ simulate (fromIPRangeDB iprs') ips
--    Right iprs' -> report $ simulate iprs' ips
    _ -> print "Can't read IP ranges database"
