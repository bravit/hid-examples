{-# LANGUAGE TemplateHaskell #-}

import DeclsGenerator
import ClientUtils
import PingTypes

import Control.Monad.Trans

$(genClientDeclsFrom "ch12/rpc/ping/RemoteFunctions.hs")

example :: RemotePing ()
example = do
    echo "Hello from client" >>= prt
    ping >>= prt
    ping >>= prt
    ping >>= prt
    ping >>= prt
    echo "Bye from client" >>= prt
  where
    prt :: Show a => a -> RemotePing ()
    prt = liftIO . print

main :: IO ()
main = runRemote (PeerAddr "localhost" 1500) example >> pure ()

