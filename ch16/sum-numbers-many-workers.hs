{-# LANGUAGE BangPatterns #-}

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Monad

oneSec :: Int
oneSec = 1000000

sendNumbers :: [Integer] -> TBMQueue Integer -> IO ()
sendNumbers xs q = do
  forM_ xs $ \x -> do
    atomically $ writeTBMQueue q x
  atomically $ closeTBMQueue q

sumNumbers :: TBMQueue Integer -> IO Integer
sumNumbers q = loop 0
  where
    loop !acc = do
      next <- atomically $ readTBMQueue q
      case next of
        Just n -> loop (acc + n)
        Nothing -> pure acc

main :: IO ()
main = do
  q <- newTBMQueueIO 10000
  summators <- replicateM 5 (async (sumNumbers q))
  _ <- async (sendNumbers [1..10000000] q)
  res <- mapM wait summators
  putStrLn $ "Partial sums are:"
  mapM_ print res
  putStrLn $ "Total sums is " ++ show (sum res)
