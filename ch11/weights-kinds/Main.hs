{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data WeightUnits = Kg | Lb

newtype Weight (u :: WeightUnits) = Weight Double
  deriving (Num, Show)

w1 :: Weight 'Kg
w1 = Weight 81

w2 :: Weight 'Lb
w2 = Weight 120

kg2lb :: Weight 'Kg -> Weight 'Lb
kg2lb (Weight wkg) = Weight (wkg * 2.205)

main :: IO ()
main = print $ kg2lb w1 + w2
