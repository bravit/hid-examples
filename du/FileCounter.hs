{-# LANGUAGE RecordWildCards #-}

module FileCounter (fileCount) where

import Control.Monad.RWS
import System.FilePath
import System.Directory
import System.PosixCompat.Files

import TraverseDir
import App

fileCount :: MyApp Int ()
fileCount = do
    AppState {..} <- get
    fs <- liftIO $ getFileStatus curPath
    when (isDirectory fs) $ do
      AppConfig {..} <- ask
      when (curDepth <= maxDepth) $ traverseDirectoryWith fileCount
      files <- liftIO $ listDirectory curPath
      tell [(curPath, length $ filterFiles ext files)]
  where
    filterFiles Nothing = id
    filterFiles (Just ext) = filter ((ext==).takeExtension)
