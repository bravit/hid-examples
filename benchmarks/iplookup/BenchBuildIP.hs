module BenchBuildIP where

import Criterion.Main
import ParseIP

import NFUtils ()

bench_buildIP :: [Benchmark]
bench_buildIP = [
    bench "buildIP-foldr" $ whnf buildIP_foldr theip
  , bench "buildIP-foldl" $ whnf buildIP_foldl theip
  , bench "buildIP-foldl-shl" $ whnf buildIP_foldl_shl theip
  ]
  where
    theip = [17,0,32,2]

bench_buildIP_list :: [Benchmark]
bench_buildIP_list = [
    bench "buildIP-foldr" $ nf (map buildIP_foldr) ipcomps
  , bench "buildIP-foldl" $ nf (map buildIP_foldl) ipcomps
  , bench "buildIP-foldl-shl" $ nf (map buildIP_foldl_shl) ipcomps
  ]
  where
    ipcomps = [ [0,0,0,1]
          , [192,168,1,1]
          , [255,255,252,41]
          , [255,255,252,41]
          , [17,0,32,2]
          ]
