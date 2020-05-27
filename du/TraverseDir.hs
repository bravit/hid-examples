module TraverseDir where

import Data.Foldable (traverse_)
import System.Directory
import System.FilePath ((</>))

import App

traverseDirectoryWith :: MyApp s () -> MyApp s ()
traverseDirectoryWith app = do
    curPath <- asks path
    content <- liftIO $ listDirectory curPath
    traverse_ go content
  where
    go name = local (enter name) app
    enter name env @ AppEnv {..} = env {
        path = path </> name,
        depth = depth + 1
      }
