{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Does it make sense to have types without values? Absolutely!

-- 'unit' is called a phantom type
newtype Weight unit = Weight Double
  deriving (Num, Show)

-- empty declarations
data Kg
data Lb

w1 :: Weight Kg
w1 = Weight 81

w2 :: Weight Lb
w2 = Weight 120

kg2lb :: Weight Kg -> Weight Lb
kg2lb (Weight wkg) =
  Weight (wkg * 2.205)

w3 :: Weight Bool -- Unfortunately, this also works
w3 = Weight 0

-- main = print $ w1 + w2 -- TYPE ERROR

main = print $ kg2lb w1 + w2

