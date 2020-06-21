{-# LANGUAGE TemplateHaskell #-}
module RemoteFunctions where

import qualified Control.Monad.State as ST
import Control.Monad.Trans

import ServerUtils

import PingTypes

$genServerDecls

echo :: String -> RemotePing String
echo msg = do
  liftIO $ putStrLn $ "Echo message: " <> msg
  pure msg

ping :: RemotePing PingAnswer
ping = do
  ST.modify (+1)
  n <- ST.get
  liftIO $ putStrLn $ "Ping received/answered with " <> show n
  pure $ PingAnswer "OK" n
