module Main where

import IPTypes
import ParseIP
import LookupIP
import GenIP

simulate :: IPRangeDB -> [IP] -> (Int, Int)
simulate iprdb ips = (yes, no)
  where
    yes = length $ filter id $ map (lookupIP iprdb) ips
    no = length ips - yes

ipdb = "data/ipdb.txt"
nReqs = 1500000

main = do
  iprs <- parseIPRanges <$> readFile ipdb
  ips <- genIPList nReqs
  case iprs of
    Right iprs -> do
                    putStr "(yes, no) = "
                    print $ simulate iprs ips
    _ -> print "Can't read IP ranges database"
