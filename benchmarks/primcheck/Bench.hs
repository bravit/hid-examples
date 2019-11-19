import Criterion.Main (defaultMain, bench, whnf)

isPrime :: Integer -> Bool
isPrime n = all notDividedBy [2 .. n-1]
  where
    notDividedBy m = n `mod` m /= 0

isPrime':: Integer -> Bool
isPrime' n = go 2
  where
    go x = case x > n-1 of
             True -> True
             False -> case n `mod` x /= 0 of
                        True -> go (x+1)
                        False -> False

isPrime'' :: Integer -> Bool
isPrime'' n = all notDividedBy [2 .. n `div` 2]
  where
    notDividedBy m = n `mod` m /= 0

primeNumber = 16183

main = defaultMain [
    bench "isPrime" $ whnf isPrime primeNumber
  , bench "isPrime'" $ whnf isPrime' primeNumber
  , bench "isPrime''" $ whnf isPrime'' primeNumber
  ]
