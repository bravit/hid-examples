module AppRWS where

import Control.Monad.RWS

data AppConfig = AppConfig {
      basePath :: FilePath,
      maxDepth :: Int,
      ext :: Maybe FilePath
    }

data AppState s = AppState {
      curDepth :: Int,
      curPath :: FilePath,
      st_field :: s
    }

type AppLog s = [(FilePath, s)]

type MyApp s = RWST AppConfig (AppLog s) (AppState s) IO

runMyApp :: MyApp s a -> AppConfig -> s -> IO (a, AppLog s)
runMyApp app config init =
  evalRWST app config (AppState 0 (basePath config) init)
