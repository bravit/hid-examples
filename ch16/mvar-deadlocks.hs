import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.IORef

action :: MVar Int -> MVar Int -> IO ()
action mv1 mv2 = do
  a <- takeMVar mv1
  b <- takeMVar mv2
  let s = a + b
  putMVar mv1 s
  putMVar mv2 s

experiment :: IORef Int -> IO ()
experiment dlcnt = do
  var1 <- newMVar 1
  var2 <- newMVar 2
  async1 <- async (action var1 var2)
  async2 <- async (action var2 var1)
  res <- try (waitBoth async1 async2)
  case res of
    Left BlockedIndefinitelyOnSTM -> modifyIORef' dlcnt (+1)
    _ -> pure ()

main :: IO ()
main = do
  dlcnt <- newIORef 0
  replicateM_ 10000 (experiment dlcnt)
  putStr "Number of deadlocks encountered: "
  readIORef dlcnt >>= print
