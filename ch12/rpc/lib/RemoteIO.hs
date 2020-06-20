{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module RemoteIO where

import DDefs
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Word
import Control.Exception (try)
import Network.Connection


import Data.Serialize hiding (get,put)
import qualified Data.ByteString as BS

runRemote :: RemoteState st => PeerAddr -> RSIO st a -> IO (Either String a)
runRemote peer computation = do
    cfg  <- remoteConnectTo peer
    res <- runRemoteCfg cfg computation
    remoteClose cfg
    return res

runRemoteCfg :: RemoteState st => RemoteConfig -> RSIO st a -> IO (Either String a)
runRemoteCfg cfg computation =
    runExceptT $
        runReaderT (evalStateT (runRem computation) initState) cfg

runRemoteCfg_ :: RemoteState st =>
                       RemoteConfig -> RSIO st a -> IO ()
runRemoteCfg_ cfg computation = runRemoteCfg cfg computation >> return ()


--send :: (Serialize a) => a -> RemoteIO ()
send :: (MonadReader RemoteConfig m, MonadIO m, Serialize a) =>
              a -> m ()
send msg = do
    cfg <- ask
    liftIO $ do
                connectionPut (handle cfg) (buildMsgEnv $ encode msg)
--                hFlush (handle cfg)
    where
        buildMsgEnv payload =
            runPut $ do
                putWord64be size
                putByteString payload
            where size = (fromIntegral $ BS.length payload) :: Word64

receive :: (Serialize a) => RSIO st a
receive = do
    cfg <- ask
    res <-liftIO $ try $ recvMsgEnv (handle cfg)
    case res of
        Left ioe -> remoteError $ "Protocol error: "++ show (ioe :: IOError)
        Right res -> either (remoteError . const "Decoding error (stage 1)") return (decode res)
    where
        recvMsgEnv h = do
            sz_msg <- connectionGet h DDefs.msgSizeField
            either fail (connectionGet h . fromIntegral) (runGet getWord64be sz_msg)

rIsEOF :: RSIO st Bool
rIsEOF = pure False {-do
    cfg <- ask
    liftIO $ hIsEOF (handle cfg) -}

remoteError :: String -> RSIO st b
remoteError err_msg = do
    rp <- remotePeerInfo
    throwError $ err_msg ++ rp

remotePeerInfo :: RSIO st String
remotePeerInfo = do
    cfg <- ask
    let peer = remotePeer cfg
    return $ " [" ++ hostname peer ++ ":" ++ show (port peer) ++ "]"

remoteConnectTo :: PeerAddr -> IO RemoteConfig
remoteConnectTo peer = do
    connCtx <- initConnectionContext
    conn <- connectTo connCtx  $ ConnectionParams (hostname peer) (port peer) Nothing Nothing
    return (RemoteConfig (PeerAddr "" 0) peer conn)

remoteClose :: MonadIO m => RemoteConfig -> m ()
remoteClose cfg = liftIO $ connectionClose (handle cfg)


