{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module AppRTWTST where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import AppTypes

newtype MyApp s a = MyApp {
      runApp :: ReaderT AppEnv
                  (WriterT (AppLog s)
                      (StateT s
                              IO)) a
    } deriving (Functor, Applicative, Monad,
                MonadIO,
                MonadReader AppEnv,
                MonadWriter (AppLog s),
                MonadState s)

runMyApp :: MyApp s a -> AppConfig -> s -> IO (a, AppLog s)
runMyApp app config st =
  evalStateT
         (runWriterT
               (runReaderT (runApp app) (initialEnv config)))
         st
