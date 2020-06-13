module IsPrime where

isPrime :: Integer -> Bool
isPrime n = all notDividedBy [2 .. n-1]
  where
    notDividedBy m = n `mod` m /= 0
