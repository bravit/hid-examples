{-# LANGUAGE OverloadedStrings #-}

module Main where

import TextShow
import Data.Text.IO as TIO
import Options.Applicative as Opt
import System.FilePath

import App
import DiskUsage
import FileCounter
import DirTree

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

printEntries :: Builder -> ((FilePath, s) -> Builder) -> AppLog s -> IO ()
printEntries title entryBuilder entries = TIO.putStr $ toText reportB
  where
    reportB = unlinesB $ title : map entryBuilder entries

simpleEntryBuilder :: TextShow s => (FilePath, s) -> Builder
simpleEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) =
  fromString (replicate (2*n) ' ') <> fromString (takeBaseName fp)

work :: AppConfig -> IO ()
work config = do
  (_, dirs) <- runMyApp dirTree config 0
  printEntries "Directory tree:" treeEntryBuilder dirs

  (_, counters) <- runMyApp fileCount config 0
  printEntries "File counter:" simpleEntryBuilder counters

  (_, usages) <- runMyApp diskUsage config 0
  printEntries "File space usage:" simpleEntryBuilder usages

main :: IO ()
main = execParser opts >>= work
  where
    opts = info (mkConfig <**> helper)
                (fullDesc <> progDesc "File space usage info")
