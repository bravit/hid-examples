module BenchBuildIP where

import Criterion.Main
import Data.Word

import NFUtils ()
import ParseIP

theip :: [Word8]
theip = [17,0,32,2]

bench_buildIP :: [Benchmark]
bench_buildIP = [
    bench "buildIP-foldr" $ nf buildIP_foldr theip
  , bench "buildIP-foldl" $ nf buildIP_foldl theip
  , bench "buildIP-foldl-shl" $ nf buildIP_foldl_shl theip
  ]
