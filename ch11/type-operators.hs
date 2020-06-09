{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

data a + b = Inl a | Inr b
  deriving Show

data a * b = a :*: b
  deriving Show

infixl 6 +
infixl 7 *

first :: a * b -> a
first (a :*: _) = a

second :: a * b -> b
second (_ :*: b) = b

val1 :: Int + Bool * Bool
val1 = Inl 0

val2 :: Int + Bool * Bool
val2 = Inr (True :*: False)

type Point a = a + a * a + a * a * a

zero2D :: Point Int
zero2D = Inl (Inr (0 :*: 0))

main :: IO ()
main = do
  print val1
  print val2
  print zero2D

