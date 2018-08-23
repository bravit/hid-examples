import Data.IORef
import Control.Monad

sumNumbers :: IO Int
sumNumbers = do
   s <- newIORef 0
   go s
   readIORef s
 where
   go s = do
     putStr "Enter next integer number (empty line to finish): "
     n <- getLine
     when (not $ null n) $ do
       let num = read n
       modifyIORef' s (+ num)
       go s

main = do
  s <- sumNumbers
  putStr "Your sum is: "
  print s
