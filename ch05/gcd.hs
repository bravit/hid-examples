import Control.Monad.Writer

gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

gcdM :: (Integral a, Monad m) => (a -> a -> m ()) -> a -> a -> m a
gcdM step a 0 = step a 0 >> pure a
gcdM step a b = step a b >> gcdM step b (a `mod` b)

gcd_print :: (Show a, Integral a) => a -> a -> IO a
gcd_print = gcdM (\a b -> print (a, b))

gcd_countSteps :: Integral a => a -> a -> Writer (Sum Int) a
gcd_countSteps = gcdM (\_ _ -> tell $ Sum 1)

gcd_logSteps :: Integral a => a -> a -> Writer [(a, a)] a
gcd_logSteps = gcdM (\a b -> tell [(a, b)])

gcd_countSteps' :: Integral a => a -> a -> Writer (Sum Int) a
gcd_countSteps' a b = mapWriter mapper (gcd_logSteps a b)
  where
    mapper (v, w) = (v, Sum $ length w)

gcd_countSteps'' :: Integral a => a -> a -> Writer (Sum Int) a
gcd_countSteps'' = (mapWriter (Sum . length <$>) .) . gcd_logSteps

main :: IO ()
main = print "OK"
