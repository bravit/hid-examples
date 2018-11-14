{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module App where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import System.Posix.Types

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

newtype MyApp s a = MyApp {
      runApp :: ReaderT AppConfig 
                  (WriterT (AppLog s)
                      (StateT (AppState s) 
                                   IO)) a
    } deriving (Functor, Applicative, Monad,
                MonadIO,
                MonadReader AppConfig,
                MonadWriter (AppLog s),
                MonadState (AppState s))

runMyApp :: MyApp s a -> AppConfig -> s -> IO (a, AppLog s)
runMyApp app config init =
  evalStateT
         (runWriterT
               (runReaderT (runApp app) config))
         (AppState 0 (basePath config) init)
