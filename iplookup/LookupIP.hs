module LookupIP where

import Data.List

import Types
import ParseIP

lookupIP ::  IPRangeDB -> IP -> Bool
lookupIP ips ip = case find (inRange ip) ips of
                    Nothing -> False
                    Just _ -> True
  where
    inRange ip (beg, end) = beg <= ip && ip <= end
