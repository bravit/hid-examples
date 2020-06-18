{-# LANGUAGE TemplateHaskell #-}
module RFDefs where

import DDefs
import ServerUtils
import DeclsGenerator
import Control.Monad.State

$genServerDecls

instance RemoteState Integer where
    initState = 0

ping :: String
ping = "OK"

ping2 :: RemoteStIO Integer String
ping2 = do
  modify (+1)
  n <- Control.Monad.State.get
  pure $ "OK("++show n++")"
