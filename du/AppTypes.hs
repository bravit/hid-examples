module AppTypes where

data AppConfig = AppConfig {
      path :: FilePath,
      maxDepth :: Int,
      ext :: Maybe FilePath,
      followSymlinks :: Bool
    }

data AppState s = AppState {
      curDepth :: !Int,
      st_field :: s
    }

type AppLog s = [(FilePath, s)]
