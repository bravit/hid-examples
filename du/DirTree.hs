module DirTree where

import App
import TraverseDir

dirTree :: MyApp Int ()
dirTree = do
    AppEnv {..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
      tell [(path, depth)]
      traverseDirectoryWith dirTree
