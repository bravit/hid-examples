{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module PingCommon where

import GHC.Generics
import Data.Serialize

import RpcCommon

instance RemoteState Integer where
    initState = 0

type RemotePing a = RSIO Integer a

data PingAnswer = PingAnswer String Integer
  deriving stock (Show, Generic)
  deriving anyclass Serialize
