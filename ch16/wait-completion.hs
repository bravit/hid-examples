import Control.Concurrent
import System.Random

oneSec :: Int
oneSec = 1000000

randomDelay :: IO ()
randomDelay = do
  secs <- getStdRandom (uniformR (1, 5))
  putStrLn $ "Waiting for " ++ show secs ++ "sec"
  threadDelay $ secs * oneSec

main :: IO ()
main = do
--  _ <- forkIO randomDelay
--  pure ()
  start <- newEmptyMVar
  fin <- newEmptyMVar
  _ <- forkFinally (takeMVar start >> randomDelay)
                   (\_ -> putMVar fin ())
  threadDelay oneSec
  putMVar start ()
  takeMVar fin
  putStrLn "Exiting..."
