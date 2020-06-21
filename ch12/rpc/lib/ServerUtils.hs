{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerUtils (serveRPC, genServerDecls, runSerialized) where

import Data.Serialize
import Data.ByteString (ByteString)
import Control.Monad
import Control.Monad.Catch
import Network.Simple.TCP
import Network.Connection
import Network.Socket (PortNumber)

import RemoteIO
import DDefs
import DeclsGenerator (genServerDecls)

runSerialized :: (Serialize a, Serialize b) =>
                 RemoteAction st a b -> ByteString -> RSIO st ByteString
runSerialized action params
  = unEitherStaged Stage2 (decode params) >>= liftM encode . action

serveRPC :: (Serialize a, RemoteState st) =>
          HostName -> PortNumber -> RPCTable st a -> IO ()
serveRPC host port funcs = serve (Host host) (show port) procRequests
  where
    connParams = ConnectionParams host port Nothing Nothing
    procRequests (connSock, sockAddr) = do
      logConnection "New connection" sockAddr
      initCtx <- initConnectionContext
      conn <- connectFromSocket initCtx connSock connParams
      catch (runRemoteConn conn serveClient >> pure ())
            (\(e :: RemoteException) ->
               logConnection (displayException e) sockAddr)

    serveClient = forever (receiveRSIO >>= call >>= sendRSIO)

    call (ctx, params) =
      maybe (unsupported $ oper ctx)
            (\func -> func params)
            (lookup (oper ctx) funcs)

    unsupported operation =
      throwRemote $ "Unsupported operation (" <> operation <> ")"

    logConnection msg sockAddr =
      putStrLn $ "LOG: " <> show sockAddr <> " " <> msg
