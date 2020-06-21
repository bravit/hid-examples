{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Control.Monad.Trans

import ClientUtils

import PingTypes

$(genClientDeclsFrom "ch12/rpc/ping/RemoteFunctions.hs")

example :: Int -> RemotePing ()
example n = do
    echo "Hello from client" >>= prt
    replicateM_ n (ping >>= prt)
    echo "Bye from client" >>= prt
  where
    prt :: Show a => a -> RemotePing ()
    prt = liftIO . print

main :: IO ()
main = runRemote (PeerAddr "localhost" 1500) (example 3) >> pure ()

