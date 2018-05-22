module Params (Params (..), cmdLineParser) where

import Data.Semigroup ((<>))
import Options.Applicative
import QuoteData

data Params = Params {
                fname :: FilePath,
                company :: String,
                prices :: Bool,
                volumes :: Bool,
                html :: Bool
              }

mkParams :: Parser Params
mkParams = Params
      <$> strArgument
          (metavar "FILE" <> help "csv-file name")
      <*> strOption
          (long "company" <> short 'c'
           <> help "stock company's name" <> value "")
      <*> switch
          (long "prices" <> short 'p' <> help "output file with prices chart")
      <*> switch
          (long "volumes" <> short 'v' <> help "output file with volumes chart")
      <*> switch
          (long "html" <> help "output file with HTML report")

cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper)
                (fullDesc <> progDesc "Stock quotes data processing")
