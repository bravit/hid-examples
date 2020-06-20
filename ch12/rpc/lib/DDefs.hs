{-# LANGUAGE GeneralizedNewtypeDeriving,FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module DDefs where
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize
import Control.Monad
import Network.Socket
import Network.Connection
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State hiding (put, get)

type Operation = String
type Parameters = BS.ByteString
type Message = BS.ByteString

msgSizeField :: Int
msgSizeField = 8 -- in bytes

data RequestContext = ReqCtx {
        oper::Operation
}

data ResponseContext = RespCtx {
     ok :: Bool,
     excInfo :: String
} deriving Show

instance Serialize RequestContext where
  put (ReqCtx op) = put op
  get = liftM ReqCtx get

instance Serialize ResponseContext where
  put RespCtx {..} = do
         put ok
         put excInfo
  get = liftM2 RespCtx get get


type RequestMessage = (RequestContext, Parameters)

type ResponseMessage = (ResponseContext, ByteString)


data PeerAddr = PeerAddr {
        hostname :: String,
        port :: PortNumber
    }

data RemoteConfig = RemoteConfig {
        localPeer :: PeerAddr,
        remotePeer :: PeerAddr,
        handle :: Connection
    }

class RemoteState a where
    initState :: a

instance RemoteState () where
    initState = ()

newtype RSIO st a = RSIO {
        runRem :: StateT st (ReaderT RemoteConfig (ExceptT String IO)) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader RemoteConfig,
                MonadError String, MonadState st)
