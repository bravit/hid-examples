import System.Environment
import System.TimeIt

isPrime :: Integer -> Bool
isPrime n = go 2
  where
    go x = case x > n-1 of
             True -> True
             False -> case n `mod` x /= 0 of
                        True -> go (x+1)
                        False -> False

main :: IO ()
main = do
  [n'] <- getArgs
  timeIt $ print $ isPrime (read n')
