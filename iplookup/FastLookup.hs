module FastLookup (FastIPRangeDB, fromIPRangeDB, lookupIP) where

import IPTypes
import Data.IntervalMap.FingerTree

newtype FastIPRangeDB = IPRDB (IntervalMap IP ())

fromIPRangeDB :: IPRangeDB -> FastIPRangeDB
fromIPRangeDB (IPRangeDB iprdb) = IPRDB $ foldr ins empty iprdb
   where
     ins (IPRange ip1 ip2) = insert (Interval ip1 ip2) ()

lookupIP :: FastIPRangeDB -> IP -> Bool
lookupIP (IPRDB imap) ip = not $ null $ search ip imap