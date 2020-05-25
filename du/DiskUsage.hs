{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module DiskUsage (diskUsage) where

import Control.Monad.RWS
import System.FilePath (takeExtension)
import System.Posix.Types (FileOffset)
import System.PosixCompat.Files (FileStatus, getFileStatus,
                                 isDirectory, isRegularFile, fileSize)

import App
import TraverseDir

type DUApp = MyApp FileOffset

diskUsage :: DUApp ()
diskUsage = do
    maxDepth <- asks maxDepth
    AppState {..} <- get
    fs <- liftIO $ getFileStatus curPath
    let isDir = isDirectory fs
        shouldLog = isDir && curDepth <= maxDepth
    when isDir $ traverseDirectoryWith diskUsage
    recordEntry curPath fs
    when shouldLog $ logDiffTS st_field

recordEntry :: FilePath -> FileStatus -> DUApp ()
recordEntry fpath fs = do
    ext <- asks ext
    when (needRec fpath ext $ isRegularFile fs) $
      addToTS $ fileSize fs
  where
--    addToTS :: FileOffset -> DUApp ()
    addToTS ofs = modify (\st -> st {st_field = st_field st + ofs})
    needRec _ Nothing _ = True
    needRec fp (Just ext) isFile = isFile && (ext == takeExtension fp)

logDiffTS :: FileOffset -> DUApp ()
logDiffTS ts = do
    AppState {..} <- get
    tell [(curPath, st_field - ts)]
