import Control.Exception
import Control.Concurrent

oneSec :: Int
oneSec = 1000000

acquire :: IO ()
acquire = do
  putStrLn "Start resource acquisition"
  threadDelay oneSec
  putStrLn "Resource is acquired"

release :: IO ()
release = do
  putStrLn "Start releasing the resource"
  threadDelay oneSec
  putStrLn "Resource is released"

use :: IO ()
use = do
  putStrLn "Begin using the resource"
  threadDelay (2 * oneSec)
  putStrLn "End using the resource"

workWithResource :: IO ()
workWithResource = do
  acquire
  use `onException` release
  release

workWithResourceSafe :: IO ()
workWithResourceSafe = uninterruptibleMask $ \restore -> do
    acquire
    restore use `onException` release
    release

experiment :: Int -> IO () -> IO ()
experiment timeout action = do
  thr <- forkIO action
  threadDelay timeout
  killThread thr
  threadDelay (2 * oneSec)

main :: IO ()
main = do
  putStrLn "Asynchronous exception during `acquire`"
  experiment (oneSec `div` 2) workWithResource

  putStrLn "\nAsynchronous exception during `use`"
  experiment (oneSec + oneSec `div` 2) workWithResource

  putStrLn "\nAsynchronous exception during `release`"
  experiment (3 * oneSec + oneSec `div` 2) workWithResource

  putStrLn "\nAsynchronous exception during `acquire`/safe"
  experiment (oneSec `div` 2) workWithResourceSafe

  putStrLn "\nAsynchronous exception during `use`/safe"
  experiment (oneSec + oneSec `div` 2) workWithResourceSafe

  putStrLn "\nAsynchronous exception during `release`/safe"
  experiment (3 * oneSec + oneSec `div` 2) workWithResourceSafe
