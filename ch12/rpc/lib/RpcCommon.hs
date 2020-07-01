{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module RpcCommon where

import Data.ByteString (ByteString)
import Network.Connection
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch

msgSizeField :: Int
msgSizeField = 8 -- in bytes

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

type Operation = String
type RemoteAction st a b = a -> RSIO st b
type RPCTable st = [(Operation, RemoteAction st ByteString ByteString)]

data DecodeStages = Stage0 | Stage1 | Stage2
  deriving Show
