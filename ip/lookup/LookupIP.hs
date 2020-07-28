module LookupIP where

import Data.List (find)

import IPTypes

lookupIP ::  IPRangeDB -> IP -> Bool
lookupIP (IPRangeDB ips) ip = case find (inRange ip) ips of
                    Nothing -> False
                    Just _ -> True
  where
    inRange ip' (IPRange beg end) = beg <= ip' && ip' <= end

reportIPs :: IPRangeDB -> [IP] -> String
reportIPs iprdb = unlines . map go
  where
    go ip = show ip ++ ": " ++ yesno (lookupIP iprdb ip)
    yesno True = "YES"
    yesno False = "NO"
