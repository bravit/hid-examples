import Data.IORef
import Text.Read (readMaybe)

sumNumbers :: IO Int
sumNumbers = do
    s <- newIORef 0
    go s
  where
    go acc = readNumber >>= processNumber acc

    readNumber = do
      putStr "Put integer number (not a number to finish): "
      readMaybe <$> getLine

    processNumber acc Nothing = readIORef acc
    processNumber acc (Just n) = modifyIORef' acc (+ n) >> go acc

main :: IO ()
main = do
  s <- sumNumbers
  putStr "Your sum is: "
  print s
