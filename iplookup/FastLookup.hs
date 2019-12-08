module FastLookup where

import IPTypes
import Data.IntervalMap.FingerTree

ipRangeDB2IntervalMap :: IPRangeDB -> IntervalMap IP ()
ipRangeDB2IntervalMap (IPRangeDB iprdb) = foldr ins empty iprdb
  where
    ins (IPRange ip1 ip2) = insert (Interval ip1 ip2) ()

fastLookupIP :: IntervalMap IP () -> IP -> Bool
fastLookupIP imap ip = not $ null $ search ip imap