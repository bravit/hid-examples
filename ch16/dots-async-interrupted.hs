import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (forever)

import System.IO (hSetBuffering, stdout, BufferMode(..))

oneSec :: Int
oneSec = 1000000

doSomethingUseful :: IO ()
doSomethingUseful = do
  threadDelay $ 5 * oneSec
  putStrLn "All done"

printDots :: Int -> IO ()
printDots msec = forever $ do
  putStrLn "."
  threadDelay msec

waitEnter :: IO ()
waitEnter = getLine >> pure ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Start doing something useful"
{-
  withAsync (printDots oneSec) $ \_ ->
    withAsync waitEnter $ \enter ->
      withAsync doSomethingUseful $ \useful ->
        waitEither_ enter useful
-}
  race_ (printDots oneSec) $
    race_ waitEnter doSomethingUseful
  putStrLn "Exiting..."
