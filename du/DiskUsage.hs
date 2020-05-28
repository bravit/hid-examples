module DiskUsage (diskUsage) where

import App
import Utils

data DUEntryAction =
    TraverseDir {dirpath :: FilePath, requireReporting :: Bool}
  | RecordFileSize {fsize :: FileOffset}
  | None

diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
  where
    decide AppEnv {..} fs
      | isDirectory fs =
            TraverseDir path (depth <= maxDepth cfg)
      | isRegularFile fs && checkExtension cfg path =
            RecordFileSize (fileSize fs)
      | otherwise = None

    processEntry TraverseDir {..} = do
      usageOnEntry <- get
      traverseDirectoryWith diskUsage
      when requireReporting $ do
        usageOnExit <- get
        tell [(dirpath, usageOnExit - usageOnEntry)]
    processEntry RecordFileSize {fsize} = modify (+fsize)
    processEntry None = pure ()
