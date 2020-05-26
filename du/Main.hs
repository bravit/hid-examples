{-# LANGUAGE OverloadedStrings #-}

module Main where

import TextShow
import Data.Text.IO as TIO
import Options.Applicative as Opt

import App
import DiskUsage
import FileCounter

mkConfig :: Opt.Parser AppConfig
mkConfig =
  AppConfig
  <$> strArgument (metavar "DIRECTORY" <> value "." <> showDefault)
  <*> option auto
      (metavar "DEPTH" <> short 'd' <> long "depth" <> value maxBound <>
       help "Display an entry for all directories DEPTH directories deep")
  <*> optional
      (strOption (metavar "EXT" <> long "extension" <> short 'e' <>
       help "Filter files by extension"))
  <*> switch
      (short 'L' <> help "Follow symlinks (OFF by default)")

printEntries :: TextShow s => Builder -> AppLog s -> IO ()
printEntries title entries = TIO.putStr $ toText reportB
  where
    entryToBuilder (fp, s) = showb s <> "\t" <> fromString fp
    reportB = unlinesB $ title : map entryToBuilder entries

work :: AppConfig -> IO ()
work config = do
  (_, usages) <- runMyApp diskUsage config 0
  printEntries "File space usage:" usages

  (_, counters) <- runMyApp fileCount config 0
  printEntries "File counter:" counters

main :: IO ()
main = execParser opts >>= work
  where
    opts = info (mkConfig <**> helper)
                (fullDesc <> progDesc "File space usage info")
