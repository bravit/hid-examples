{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module DiskUsage (diskUsage) where

import System.FilePath (takeExtension)
import System.Posix.Types (FileOffset)
import System.PosixCompat.Files

import App
import TraverseDir

type DUApp = MyApp FileOffset

diskUsage :: DUApp ()
diskUsage = do
    currentPath <- asks path
    usageOnEntry <- gets st_field

    fs <- currentPathStatus
    let isDir = isDirectory fs
    when isDir $ traverseDirectoryWith diskUsage

    recordEntry currentPath (isRegularFile fs) (fileSize fs)

    maxDepth <- asks maxDepth
    curDepth <- gets curDepth
    when (isDir && curDepth <= maxDepth) $ do
      usageOnExit <- gets st_field
      tell [(currentPath, usageOnExit - usageOnEntry)]
  where
    recordEntry fpath isFile fsize = do
      ext <- asks ext
      when (needRec fpath ext isFile) $
        addToTS fsize

--    addToTS :: FileOffset -> DUApp ()
    addToTS ofs = modify (\st -> st {st_field = st_field st + ofs})

    needRec _ Nothing _ = True
    needRec fp (Just ext) isFile = isFile && (ext == takeExtension fp)
