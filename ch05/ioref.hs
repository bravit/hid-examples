import Data.IORef
import Text.Read (readMaybe)

sumNumbers :: IO Int
sumNumbers = do
    sum <- newIORef 0
    go sum
  where
    go sum = readNumber >>= processNumber sum

    readNumber = do
      putStr "Put integer number (not a number to finish): "
      readMaybe <$> getLine

    processNumber sum Nothing = readIORef sum
    processNumber sum (Just n) = modifyIORef' sum (+ n) >> go sum

main = do
  sum <- sumNumbers
  putStr "Your sum is: "
  print sum
