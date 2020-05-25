module AppTypes where

data AppConfig = AppConfig {
      basePath :: FilePath,
      maxDepth :: Int,
      ext :: Maybe FilePath
    }

data AppState s = AppState {
      curDepth :: !Int,
      curPath :: !FilePath,
      st_field :: s
    }

type AppLog s = [(FilePath, s)]
