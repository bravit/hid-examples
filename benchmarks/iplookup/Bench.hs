{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}
import Criterion.Main

import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Either (fromRight)
import Data.Maybe (fromJust)

import ParseIP
import IPTypes
import LookupIP

import Paths_hid_examples (getDataFileName)

deriving instance Generic IP
deriving instance Generic IPRange
deriving instance Generic IPRangeDB
deriving instance NFData IP
deriving instance NFData IPRange
deriving instance NFData IPRangeDB

envIPRDBFileSmall = getDataFileName "data/benchmarks/iplookup/1.iprs"
               >>= readFile
envIPRDBFileMiddle = getDataFileName "data/benchmarks/iplookup/2.iprs"
               >>= readFile
envIPRDBFileLarge = getDataFileName "data/benchmarks/iplookup/3.iprs"
               >>= readFile

envIPRDB = parseValidIPRanges <$> envIPRDBFileLarge

iptexts = [ "0.0.0.1"
          , "192.168.1.1"
          , "255.255.252.41"
          , "255.255.252.42"
          , "17.0.32.2"
          ]

ips = map (\s -> (s, fromJust $ parseIP s)) iptexts

main = defaultMain [
  bgroup "parseIP" $ map (\ip -> bench ip $ nf parseIP ip) iptexts
  , env envIPRDBFileSmall $ \ iprdbf ->
      bgroup "small ranges file" [ bench "parseIPRanges" $ nf parseIPRange iprdbf ]
  , env envIPRDBFileMiddle $ \ iprdbf ->
      bgroup "middle-sized ranges file" [ bench "parseIPRanges" $ nf parseIPRange iprdbf ]
  , env envIPRDBFileLarge $ \ iprdbf ->
      bgroup "large ranges file" [ bench "parseIPRanges" $ nf parseIPRange iprdbf ]
  , env envIPRDB $ \ iprdb ->
      bgroup "with IPRDB" $
        map (\ (textip, ip) -> bench textip $ whnf (lookupIP iprdb) ip) ips
  ]
