{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TempPhantom where

-- Does it make sense to have types without values? Absolutely!

-- 'unit' is called a phantom type
newtype Temp unit = Temp Double
  deriving (Num, Show)

-- empty declarations
data F
data C

t1 :: Temp F
t1 = 86

t2 :: Temp C
t2 = 30

f2c :: Temp F -> Temp C
f2c (Temp f) = Temp ((f-32)*5/9)

diff :: Temp u -> Temp u -> Temp u
diff (Temp t) (Temp s) = Temp (t - s)

t3 :: Temp Bool
t3 = Temp 0

zero :: Temp C
zero = diff (f2c t1) t2

-- TYPE ERROR: Couldn't match type ‘C’ with ‘F’
--err :: IO ()
--err = print $ diff t1 t2
