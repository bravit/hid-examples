module BenchBuildIPGroups where

import Criterion.Main

import NFUtils ()
import ParseIP

bench_buildIP :: [Benchmark]
bench_buildIP = [
    bgroup "buildIP" [
      let theip = [17,0,32,2] in
      bgroup "single" [
        bench "default" $ nf buildIP theip
      , bench "foldr" $ nf buildIP_foldr theip
      , bench "foldl" $ nf buildIP_foldl theip
      , bench "foldl-shl" $ nf buildIP_foldl_shl theip
      ]

    , let ipcomps = [[0,0,0,1], [192,168,1,1], [17,0,32,2],
                     [255,255,252,41], [255,255,252,41]] in
      bgroup "several" [
        bench "default" $ nf (map buildIP) ipcomps
      , bench "foldr" $ nf (map buildIP_foldr) ipcomps
      , bench "foldl" $ nf (map buildIP_foldl) ipcomps
      , bench "foldl-shl" $ nf (map buildIP_foldl_shl) ipcomps
      ]
    ]
  ]
