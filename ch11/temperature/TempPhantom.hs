{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TempPhantom where

-- Does it make sense to have types without values? Absolutely!

-- 'unit' is called a phantom type
newtype Temp unit = Temp Double
  deriving Num

-- empty declarations
data F
data C

paperBurning :: Temp F
paperBurning = 451

absoluteZero :: Temp C
absoluteZero = -273

tf :: Temp F
tf = 86

tc :: Temp C
tc = 30

f2c :: Temp F -> Temp C
f2c (Temp f) = Temp ((f-32)*5/9)

tb :: Temp Bool
tb = Temp 0

zero :: Temp C
zero = f2c tf - tc

-- TYPE ERROR: Couldn't match type ‘C’ with ‘F’
--err :: IO ()
--err = print $ tf - tc
