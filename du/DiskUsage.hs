{-# LANGUAGE FlexibleContexts #-}

module DiskUsage (diskUsage) where

import System.FilePath (takeExtension)
import System.Posix.Types (FileOffset)
import System.PosixCompat.Files

import App
import TraverseDir

type DUApp = MyApp FileOffset

data DUEntryInfo = Dir | File FileOffset | Other

currentEntryInfo :: DUApp DUEntryInfo
currentEntryInfo = do
  fs <- currentPathStatus
  pure $
    if isDirectory fs then Dir
    else if isRegularFile fs then (File $ fileSize fs)
    else Other

diskUsage :: DUApp ()
diskUsage = do
    curPath <- asks path
    curDepth <- gets currentDepth
    maxDepth <- asks maximumDepth
    info <- currentEntryInfo
    case info of
      Dir -> processDirectory curPath (curDepth <= maxDepth)
      File fsize -> recordFile curPath fsize
      Other -> pure ()
  where
    processDirectory curPath requiresReporting = do
      usageOnEntry <- gets st_field
      traverseDirectoryWith diskUsage
      when requiresReporting $ do
        usageOnExit <- gets st_field
        tell [(curPath, usageOnExit - usageOnEntry)]

    recordFile curPath fsize = do
      ext <- asks extension
      when (needRec curPath ext) $
        addToTotalSize fsize

    needRec fp = maybe True (\ext -> ext == takeExtension fp)

--    addToTotalSize :: FileOffset -> DUApp ()
    addToTotalSize ofs = modify (\st -> st {st_field = st_field st + ofs})
