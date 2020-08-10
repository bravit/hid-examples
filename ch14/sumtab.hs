{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Streaming as S
import qualified Streaming.Prelude as S
import qualified Data.List.Extra as LE
import System.Environment
import Control.Monad (foldM)
import Data.Text (Text)
import Data.Text.IO as TIO
import TextShow

withTab :: Int -> Text
withTab num = showt num <> "\t"

tabulateL :: Int -> [Int] -> [Text]
tabulateL cols ns = map mconcat $ LE.chunksOf cols $ map withTab ns

sumAndTabL :: Int -> [Int] -> IO Int
sumAndTabL cols ns = do
  mapM_ TIO.putStrLn $ tabulateL cols ns
  pure $ sum ns

sumAndTabL1 :: Int -> [Int] -> IO Int
sumAndTabL1 cols ns = foldM prtLineSum 0 $ LE.chunksOf cols ns
  where
    prtLineSum !acc xs = do
      TIO.putStrLn $ mconcat $ map withTab xs
      pure $ acc + sum xs

tabulateS :: Int -> Stream (Of Int) IO r -> Stream (Of Text) IO r
tabulateS cols str = mapsM S.mconcat $ S.chunksOf cols $ S.map withTab str

sumAndTabS :: Int -> Stream (Of Int) IO r -> IO Int
sumAndTabS cols =
  fmap S.fst' . S.mapM_ TIO.putStrLn . tabulateS cols . S.store S.sum

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-l"] -> sumAndTabL 3 [1..10] >>= print
    ["-l1"] -> sumAndTabL1 5 [1..300000] >>= print
    ["-s"] -> sumAndTabS 5 (S.each [1..300000]) >>= print
    _ -> TIO.putStrLn "Unsupported args"
