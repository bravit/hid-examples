module Params (Params (..), cmdLineParser) where

import Options.Applicative
import Data.Text (Text, strip)

data Params = Params {
                fname :: FilePath
              , company :: Maybe Text
              , chart :: Bool
              , htmlFile :: Maybe FilePath
              , silent :: Bool
              }

mkParams :: Parser Params
mkParams =
  Params <$>
             strArgument
               (metavar "FILE" <> help "CSV file name")
         <*> optional (strip <$> strOption
               (long "name" <> short 'n' <>
                help "Company name "))
         <*> switch
               (long "chart" <> short 'c' <>
                help "Generate chart")
         <*> optional (strOption $
               long "html" <> metavar "FILE" <>
               help "Generate HTML report")
         <*> switch
               (long "silent" <> short 's' <>
                help "Don't print statistics")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper)
                (fullDesc <> progDesc "Stock quotes data processing")
