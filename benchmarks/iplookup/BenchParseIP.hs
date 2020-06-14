module BenchParseIP where

import Criterion.Main

import ParseIP

import NFUtils ()
import Data

bench_parseIP :: [Benchmark]
bench_parseIP = [
    bench "parseIP" $ nf (map parseIP) iptexts
  , bgroup "parseIP" [
      bench "monadic" $ nf (map parseIPMonadic) iptexts
    , bench "iterative" $ nf (map parseIPIter) iptexts
    , bench "iterative-strict" $ nf (map parseIPIterStrict) iptexts
    ]
  ]
