{-# LANGUAGE RecordWildCards #-}

module TraverseDir where

import Data.Foldable (traverse_)
import Control.Monad.RWS
import System.Directory
import System.FilePath ((</>))
import System.PosixCompat.Files

import App

traverseDirectoryWith :: MyApp s () -> MyApp s ()
traverseDirectoryWith app = do
    curPath <- asks path
    content <- liftIO $ listDirectory curPath
    modify incDepth
    traverse_ go $ map (curPath </>) content
    modify decDepth
  where
    go newPath = local (withPath newPath) app
    withPath newPath cfg = cfg { path = newPath }
    incDepth st @ AppState {..} = st {curDepth = curDepth + 1}
    decDepth st @ AppState {..} = st {curDepth = curDepth - 1}

currentPathStatus :: MyApp s FileStatus
currentPathStatus = do
  AppConfig {..} <- ask
  liftIO $ if followSymlinks
             then getFileStatus path
             else getSymbolicLinkStatus path
