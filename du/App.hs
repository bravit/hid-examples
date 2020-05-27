module App (module E, MyApp, runMyApp, currentPathStatus, checkExtension) where

import Control.Monad as E
import Control.Monad.Trans as E
import Control.Monad.Reader as E
import Control.Monad.Writer as E
import Control.Monad.State as E
import System.PosixCompat.Files as E
import System.FilePath
import AppTypes as E

import AppRWST
--import AppRTWTST

currentPathStatus :: MyApp s FileStatus
currentPathStatus = do
  AppEnv {fileStatus, path} <- ask
  liftIO $ fileStatus path

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg fp =
  maybe True (`isExtensionOf` fp) (extension cfg)
