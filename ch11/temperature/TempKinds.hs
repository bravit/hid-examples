{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TempKinds where

data TempUnits = F | C

newtype Temp (u :: TempUnits) = Temp Double
  deriving (Num, Show)

t1 :: Temp 'F
t1 = 86

t2 :: Temp 'C
t2 = 30

f2c :: Temp 'F -> Temp 'C
f2c (Temp f) = Temp ((f-32)*5/9)

diff :: Temp u -> Temp u -> Temp u
diff (Temp t) (Temp s) = Temp (t - s)

-- TYPE ERROR: Expected kind ‘TempUnits’, but ‘Bool’ has kind ‘*’
--t3 :: Temp Bool
--t3 = Temp 0

zero :: Temp 'C
zero = diff (f2c t1) t2

-- TYPE ERROR: Couldn't match type ‘C’ with ‘F’
--err :: IO ()
--err = print $ diff t1 t2
