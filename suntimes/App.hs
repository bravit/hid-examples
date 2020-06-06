{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module App where

import Control.Monad.Reader
import Control.Monad.Catch

import Types

newtype MyApp a = MyApp {
      runApp :: ReaderT WebAPIAuth IO a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadThrow, MonadCatch, MonadMask,
                MonadReader WebAPIAuth)

runMyApp :: MyApp a -> WebAPIAuth -> IO a
runMyApp app config = runReaderT (runApp app) config
