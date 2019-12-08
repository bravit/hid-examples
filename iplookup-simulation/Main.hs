module Main where

import IPTypes
import ParseIP
import LookupIP
import System.Random

ipdb = "data/ipdb.txt"
nReqs = 1000000

simulateWith :: IPRangeDB -> Int -> (Int, Int)
simulateWith iprdb nReqs = (yes, no)
  where
    ips = genIPList nReqs
    yes = length $ filter id $ map (lookupIP iprdb) ips
    no = nReqs - yes

genIPList :: Int -> [IP]
genIPList n = map IP $ take n $ iterate (+step) 0
  where
    step = fromIntegral $ 2^32 `div` n

report :: (Int, Int) -> IO ()
report info = putStrLn $ "(yes, no) = " ++ show info

main = do
  iprs <- parseIPRanges <$> readFile ipdb
  case iprs of
    Right iprs -> report (simulateWith iprs nReqs)
    _ -> print "Can't read IP ranges database"