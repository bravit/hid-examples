{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

sum_prod :: Num a => a -> a -> (# a, a #)
sum_prod a b = (# a + b, a*b #)

test_sum_prod :: IO ()
test_sum_prod = case sum_prod (5 :: Int) 6 of
                  (# s, p #) -> print (s + p)

fib_next :: Int -> (Integer, Integer) -> (Integer, Integer)
fib_next 0 p = p
fib_next n (a, b) = fib_next (n-1) (b, a+b)

test_fib_next :: IO ()
test_fib_next = case fib_next 100 (0, 1) of
         (a, _) -> print a

fib_next' :: Int -> (# Integer, Integer #) -> (# Integer, Integer #)
fib_next' 0 p = p
fib_next' n (# a, b #) = fib_next' (n-1) (# b, a+b #)

test_fib_next' :: IO ()
test_fib_next' = case fib_next' 100 (# 0, 1 #) of
         (# a, _ #) -> print a

type Coordinates = (# Double | (# Double, Double #) #)

smallest :: Coordinates -> Double
smallest (# d | #) = d
smallest (# | (# x, y #) #) = min x y

test_smallest :: IO ()
test_smallest = do
  print $ smallest (# 5.4 | #)
  print $ smallest (# | (# 2.3, 1.2 #) #)

main :: IO ()
main = do
  test_fib_next
  test_fib_next'
  test_sum_prod
  test_smallest
