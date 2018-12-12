{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

import Options.Applicative as Opt
import Data.Aeson
import Control.Exception.Safe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup ((<>)) -- required for GHC 8.2
import qualified Data.ByteString as B
import System.Exit
import System.IO.Error (isDoesNotExistError, ioeGetFileName)

import App
import ProcessRequest
import STExcept

data AppMode = FileInput FilePath | Interactive
data Params = Params
                AppMode -- mode 
                FilePath -- config file

mkParams :: Opt.Parser Params
mkParams = Params <$> (fileInput <|> interactive) <*> config
  where
    fileInput = FileInput <$> strOption
                (long "file" <> short 'f' <>
                 metavar "FILENAME" <> help "Input file" )
    interactive = flag Interactive Interactive
                  (long "interactive" <> short 'i' <>
                   help "Interactive mode")
    config = strOption (long "conf" <> short 'c' <>
                 value "config.json" <>
                 showDefault <>
                 metavar "CONFIGNAME" <> help "Configuration file" )

withConfig :: Params -> IO ()
withConfig (Params appMode config) = do
    wauth <- eitherDecodeStrict <$> B.readFile config
    case wauth of
      Right wauth' -> runMyApp (run appMode) wauth'
      Left err -> putStrLn $ "Error reading configuration file: " ++ err
  where
    run (FileInput fname) = liftIO (TIO.readFile fname) >>= processMany . T.lines
    run Interactive = processInteractively

main = do
  (execParser opts >>= withConfig)
       `catches` [Handler parserExit,
                  Handler printIOError,
                  Handler printSunInfoError,
                  Handler printOtherErrors]
  where
    opts =
      info (mkParams <**> helper)
           (fullDesc <>
            progDesc "Reports sunrise/sunset times for the specified location")
    parserExit :: ExitCode -> IO ()
    parserExit _ = pure ()
    printIOError :: IOException -> IO ()
    printIOError e
      | isDoesNotExistError e = do
           let mbfn = ioeGetFileName e
           putStrLn $ "File " ++ maybe "" id mbfn  ++ " not found"
      | otherwise = putStrLn $ "I/O error: " ++ show e
    printSunInfoError :: SunInfoException -> IO ()
    printSunInfoError = print
    printOtherErrors :: SomeException -> IO ()
    printOtherErrors _ = putStrLn "Unknown error. Please, try again later."
