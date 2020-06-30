{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module RpcCommon where

import Data.ByteString (ByteString)
import Data.Serialize
import GHC.Generics
import Network.Socket
import Network.Connection
import Control.Monad.Reader
import Control.Monad.State hiding (put, get)
import Control.Monad.Catch

type Operation = String

msgSizeField :: Int
msgSizeField = 8 -- in bytes

data RequestContext = ReqCtx {
        oper::Operation
  }
  deriving stock Generic
  deriving anyclass Serialize

type RequestMessage = (RequestContext, ByteString)

type ResponseMessage = Either String ByteString

data PeerAddr = PeerAddr {
        hostname :: String,
        port :: PortNumber
    }

data RemoteException =
    ConnectionClosed
  | RemoteException String

instance Show RemoteException where
    show ConnectionClosed = "Connection closed"
    show (RemoteException msg) = "Remote Exception: " <> msg

instance Exception RemoteException

class RemoteState a where
    initState :: a

instance RemoteState () where
    initState = ()

newtype RSIO st a = RSIO {
        runRem :: StateT st (ReaderT Connection IO) a
    } deriving newtype (Functor, Applicative, Monad, MonadIO,
                        MonadReader Connection,
                        MonadState st,
                        MonadThrow, MonadCatch)

type RemoteAction st a b = a -> RSIO st b

type RPCTable st a = [(Operation, RemoteAction st a ByteString)]

data DecodeStages = Stage0 | Stage1 | Stage2
  deriving Show
