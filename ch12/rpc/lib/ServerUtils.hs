{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerUtils where
import Data.Serialize
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Loops
import Control.Exception
import Network.Simple.TCP
import Network.Connection
import Network.Socket

import RemoteIO
import DDefs

type RemoteAction st a b = a -> RemoteStIO st b

run_serialized :: (Serialize a, Serialize b) => RemoteAction st a b -> Parameters -> RemoteStIO st ByteString
run_serialized action params
            = either
                (remoteError . ("Decoding error (stage 2): "++))
                (liftM encode . action)
                (decode params)

serve' :: (Serialize a, RemoteState st) =>
          PortNumber -> [(Operation, RemoteAction st a ByteString)] -> IO a
serve' port funcs = serve (Host "127.0.0.1") (show port) procRequests
    where
        procRequests (connhdl, sockAddr) = do
                logConnection -- clientHost clientPort
                initCtx <- initConnectionContext
                conn <- connectFromSocket initCtx connhdl (ConnectionParams "localhost" port Nothing Nothing)
                catch (runRemoteCfg_
                           (RemoteConfig
                               (PeerAddr "localhost" port)
                               (PeerAddr "localhost" port)
                               conn)
                           (untilM_ serveClient rIsEOF)) (\(ioe :: IOError) -> putStrLn $ show ioe)
                pure ()

        serveClient = receive >>= call >>= RemoteIO.send

        call (ctx, params) =
          maybe (return $ unsupported ctx)
                (\f -> liftM (RespCtx True "",) $ f params)
                (lookup (oper ctx) funcs)

        unsupported ctx = (RespCtx False (oper ctx ++ ": unsupported operation"), BS.empty)

        logConnection {-clientHost clientPort-} =
            putStrLn $ "New connection from " -- ++ clientHost ++ ":" ++ show clientPort
