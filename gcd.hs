import Control.Monad.Writer

gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

gcd_countSteps :: Integer -> Integer -> Writer (Sum Int) Integer
gcd_countSteps a 0 = tell (Sum 1) >> pure a
gcd_countSteps a b = tell (Sum 1) >> gcd_countSteps b (a `mod` b)

gcd_logSteps :: Integer -> Integer -> Writer [(Integer, Integer)] Integer
gcd_logSteps a 0 = tell [(a,0)] >> pure a
gcd_logSteps a b = tell [(a,b)] >> gcd_logSteps b (a `mod` b)

gcd_countSteps' a b = mapWriter mapper (gcd_logSteps a b)
  where
    mapper (a, w) = (a, Sum $ length w)

gcd_countSteps'' = (mapWriter (Sum . length <$>) .) . gcd_logSteps

main = print "OK"
