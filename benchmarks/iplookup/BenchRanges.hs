module BenchRanges where

import Criterion.Main

import ParseIP

import NFUtils ()
import Data

bench_ranges :: [Benchmark]
bench_ranges = [
  bgroup "ranges" [
      bgroup "read" $
        map (\(desc, fname) ->
              bench desc $ nfIO (readIPRDBFile fname))
            rangeFiles
    , bgroup "parse" $
        map (\(desc, fname) ->
              env (readIPRDBFile fname) $
                \iprdbf -> bench desc $ nf parseIPRange iprdbf)
            rangeFiles
    ]
  ]
  where
    rangeFiles = [("small", "1.iprs"),
                      ("middle-sized", "2.iprs"),
                      ("large", "3.iprs")]
