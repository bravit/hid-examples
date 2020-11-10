import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Text.Read (readMaybe)

maxThread :: Int
maxThread = 5

waitUnless :: Int -> TVar Int -> STM ()
waitUnless n tv = do
  n' <- readTVar tv
  check $ n == n'

hello :: Int -> TVar Int -> IO ()
hello n tv = forever $ do
  atomically $ waitUnless n tv
  putStrLn $ "Hello from thread " ++ show n
  atomically $ writeTVar tv 0

userLoop :: TVar Int -> IO ()
userLoop tv = do
  atomically $ waitUnless 0 tv
  putStrLn $ "Enter thread number (1.." ++ show maxThread ++ "):"
  n' <- readMaybe <$> getLine
  case n' of
    Nothing -> pure ()
    Just n -> when (1 <= n && n <= maxThread) $ do
      atomically $ writeTVar tv n
      userLoop tv

main :: IO ()
main = do
  tv <- atomically $ newTVar 0
  forM_ [1..maxThread] $ \n -> async $ hello n tv
  userLoop tv
