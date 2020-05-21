import Control.Monad.Writer
import Data.Bifunctor

gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

gcdM :: (Integral a, Monad m) => (a -> a -> m ()) -> a -> a -> m a
gcdM step a 0 = step a 0 >> pure a
gcdM step a b = step a b >> gcdM step b (a `mod` b)

gcd_print = gcdM (\a b -> print (a, b))

gcd_countSteps :: Integral a => a -> a -> Writer (Sum Int) a
gcd_countSteps = gcdM (\_ _ -> tell $ Sum 1)

gcd_logSteps :: Integral a => a -> a -> Writer [(a, a)] a
gcd_logSteps = gcdM (\a b -> tell [(a, b)])

gcd_countSteps' a b = mapWriter mapper (gcd_logSteps a b)
  where
    mapper (a, w) = (a, Sum $ length w)

gcd_countSteps'' = (mapWriter (Sum . length <$>) .) . gcd_logSteps

main = print "OK"
