import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan
import Control.Monad
import Text.Read

publisher :: TMChan Int -> IO ()
publisher ch = do
  putStrLn "Enter next number: "
  x' <- readMaybe <$> getLine
  case x' of
    Just x -> do
      atomically $ writeTMChan ch x
      publisher ch
    Nothing -> atomically $ closeTMChan ch

printer :: TQueue (Int, Int) -> IO ()
printer q = forever $ do
  (n, x) <- atomically $ readTQueue q
  putStrLn $ "Subscriber " ++ show n ++
             " received number " ++ show x

subscriber :: Int -> TMChan Int -> TQueue (Int, Int) -> IO ()
subscriber n inch outq = loop
  where
    loop = do
      next <- atomically $ readTMChan inch
      case next of
        Just x -> do
          atomically $ writeTQueue outq (n, x)
          loop
        Nothing -> pure ()

spawnSubscribers
  :: Int -> TMChan Int -> TQueue (Int, Int) -> IO [Async ()]
spawnSubscribers total ch outq =
  forM [1..total] $ \n -> do
    inch <- atomically $ dupTMChan ch
    async $ subscriber n inch outq

main :: IO ()
main = do
  outq <- newTQueueIO
  withAsync (printer outq) $ \_ -> do
    ch <- newBroadcastTMChanIO
    subs <- spawnSubscribers 3 ch outq
    publisher ch
    mapM_ wait subs
  putStrLn "Exiting..."
