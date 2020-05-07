module Params (Params (..), cmdLineParser) where

import Data.Semigroup ((<>))
import Options.Applicative

data Params = Params {
                fname :: FilePath
              , company :: String
              , chart :: Bool
              , html :: Bool
              , no_text :: Bool
              }

mkParams :: Parser Params
mkParams =
  Params <$>
             strArgument
             (metavar "FILE" <> help "CSV file name")
         <*> strOption
             (long "ticker" <> short 't'
              <> help "stock company's ticker" <> value "")
         <*> switch
             (long "chart" <> short 'c' <>
              help "create file with prices chart")
         <*> switch
             (long "html" <>
              help "create file with HTML report")
         <*> switch
             (long "no-text" <> short 'n' <>
              help "don't print statistics report")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper)
                (fullDesc <> progDesc "Stock quotes data processing")
