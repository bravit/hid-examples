{-# LANGUAGE RecordWildCards #-}

module FileCounter (fileCount) where

import System.FilePath (takeExtension)
import System.Directory (listDirectory)
import System.PosixCompat.Files (isDirectory)

import App
import TraverseDir

fileCount :: MyApp Int ()
fileCount = do
    curDepth <- gets curDepth
    AppConfig {..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && curDepth <= maxDepth) $ do
      files <- liftIO $ listDirectory path
      tell [(path, length $ filterFiles ext files)]
      traverseDirectoryWith fileCount
  where
    filterFiles Nothing = id
    filterFiles (Just ext) = filter ((ext==).takeExtension)
