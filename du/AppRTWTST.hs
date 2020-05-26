{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module AppRTWTST where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import AppTypes

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
runMyApp app config initState =
  evalStateT
         (runWriterT
               (runReaderT (runApp app) config))
         (AppState 0 initState)
