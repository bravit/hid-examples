module AppRWST where

import Control.Monad.RWS
import AppTypes

type MyApp s = RWST AppConfig (AppLog s) (AppState s) IO

runMyApp :: MyApp s a -> AppConfig -> s -> IO (a, AppLog s)
runMyApp app config initial =
  evalRWST app config (AppState 0 (basePath config) initial)
