{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when)
import System.Environment

import Fmt

type Entry = (Text, Int)
type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null)
         $ map cleanWord $ T.words t
    buildEntry ws@(w:_) = (w, length ws)
    cleanWord = T.dropAround (not . isLetter)

allWords :: Vocabulary -> [Text]
allWords vocab = map fst vocab

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocab = (sum $ map snd vocab, length vocab)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

allWordsReport :: Vocabulary -> Text
allWordsReport vocab =
  fmt $ nameF "All words" $ unlinesF (allWords vocab)

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab = fmt $ 
     "Total number of words: " +|total|+
     "\nNumber of unique words: " +|unique|+ "\n"
  where
    (total, unique) = wordsCount vocab

wordsCountReport' :: Vocabulary -> Text 
wordsCountReport' vocab = T.unlines [part1, part2]
  where
    (total, unique) = wordsCount vocab
    part1 = T.append (T.pack "Total number of words: ")
                     (T.pack $ show total)
    part2 = T.append (T.pack "Number of unique words: ")
                     (T.pack $ show unique)

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab n =
    fmt $ nameF "Frequent words"
        $ blockListF' "" fmtEntry reportData
  where
    reportData = take n $ wordsByFrequency vocab
    fmtEntry (t, n) = ""+|t|+": "+|n|+""

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname allWords n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  when allWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", fname, num] ->
      processTextFile fname True (read num)
    [fname, num] ->
      processTextFile fname False (read num)
    _ -> putStrLn "Usage: vocab3 [-a] filename freq_words_num"
