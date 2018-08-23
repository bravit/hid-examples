{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import Data.Foldable
import Control.Monad.Writer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid

data ErrorMsg = WrongFormat
                   Int
                   T.Text
  deriving Show

type SQL = T.Text

genInsert s1 s2 = "INSERT INTO items VALUES ('" <> s1 <> "','" <> s2 <> "');\n"

processOneLine :: (Int, T.Text) -> Writer [ErrorMsg] SQL 
processOneLine (i, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processOneLine (i, s) = writer (T.empty, [WrongFormat i s])

genSQL :: T.Text -> Writer [ErrorMsg] SQL
genSQL t = T.concat <$> traverse processOneLine (zip [1..] $ T.lines t)


testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL = do
  let (sql, errors) = runWriter (genSQL testData) 
  TIO.putStrLn "SQL:"
  TIO.putStr sql
  TIO.putStrLn "Errors:"
  traverse_ print errors

main = testGenSQL
