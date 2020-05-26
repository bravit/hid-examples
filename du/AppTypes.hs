module AppTypes where

data AppConfig = AppConfig {
      path :: FilePath,
      maximumDepth :: Int,
      extension :: Maybe FilePath,
      followSymlinks :: Bool
    }

data AppState s = AppState {
      currentDepth :: !Int,
      st_field :: !s
    }

type AppLog s = [(FilePath, s)]
