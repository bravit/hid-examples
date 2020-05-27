module FileCounter (fileCount) where

import System.Directory.Extra (listFiles)

import App
import TraverseDir

fileCount :: MyApp Int ()
fileCount = do
    AppEnv {..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
      traverseDirectoryWith fileCount
      files <- liftIO $ listFiles path
      tell [(path, length $ filter (checkExtension cfg) files)]
