import System.Environment
import System.TimeIt

isPrime :: Integer -> Bool
isPrime n = all notDividedBy [2 .. n-1]
  where
    notDividedBy m = n `mod` m /= 0

main :: IO ()
main = do
  [n'] <- getArgs
  timeIt $ print $ isPrime (read n')
