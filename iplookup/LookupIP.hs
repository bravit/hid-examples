module LookupIP where

import Data.List

import IPTypes
import ParseIP

lookupIP ::  IPRangeDB -> IP -> Bool
lookupIP (IPRangeDB ips) ip = case find (inRange ip) ips of
                    Nothing -> False
                    Just _ -> True
  where
    inRange ip (IPRange beg end) = beg <= ip && ip <= end
