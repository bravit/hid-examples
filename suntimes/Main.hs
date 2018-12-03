import Options.Applicative as Opt
import Data.Aeson
import Control.Exception.Safe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad
import Data.Semigroup ((<>)) -- required for GHC 8.2
import qualified Data.ByteString as B
import System.Exit
import System.IO.Error (isDoesNotExistError, ioeGetFileName)

import Types
import GeoCoordsReq
import SunTimes
import ProcessRequest
import STExcept

data AppMode = FileInput FilePath | Interactive
data Params = Params AppMode FilePath

mkAppMode :: Opt.Parser AppMode
mkAppMode = fileInput <|> interactive
  where
    fileInput = FileInput <$> strOption
                (long "file" <> short 'f' <>
                 metavar "FILENAME" <> help "Input file" )
    interactive = flag Interactive Interactive
                  (long "interactive" <> short 'i' <>
                   help "Interactive mode")

mkParams :: Opt.Parser Params
mkParams = Params <$>
             mkAppMode <*>
             strOption
                (long "conf" <> short 'c' <>
                 value "config.json" <>
                 showDefault <>
                 metavar "CONFIGNAME" <> help "Configuration file" )

withConfig :: Params -> IO ()
withConfig (Params appMode config) = do
    wauth <- eitherDecodeStrict `fmap` B.readFile config
    case wauth of
      Right wauth' -> run wauth' appMode
      Left err -> putStrLn $ "Error reading configuration file: " ++ err
  where
    run wauth (FileInput fname) = TIO.readFile fname >>= processMany wauth . T.lines
    run wauth Interactive = processInteractively wauth

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
            progDesc "Reporting sunrise/sunset times for specified location")
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
