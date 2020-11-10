{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forM_)

oneSec :: Int
oneSec = 1000000

sendNumbers :: [Int] -> Chan (Maybe Int) -> IO ()
sendNumbers xs ch = do
  forM_ xs $ \x -> do
    writeChan ch (Just x)
    threadDelay $ oneSec `div` 2
  writeChan ch Nothing

sumNumbers :: Chan (Maybe Int) -> IO Int
sumNumbers ch = loop 0
  where
    loop !acc = do
      next <- readChan ch
      case next of
        Just n -> do
          putStrLn $ "We've got a number: " ++ show n
          loop (acc + n)
        Nothing -> do
          putStrLn "There are no more numbers"
          pure acc

main :: IO ()
main = do
  ch <- newChan
  summator <- async (sumNumbers ch)
  _ <- async (sendNumbers [1..5] ch)
  res <- wait summator
  putStrLn $ "Sum is " ++ show res
