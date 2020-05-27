module AppRWST where

import Control.Monad.RWS
import AppTypes

type MyApp s = RWST AppEnv (AppLog s) s IO

runMyApp :: MyApp s a -> AppConfig -> s -> IO (a, AppLog s)
runMyApp app config st = evalRWST app (initialEnv config) st
