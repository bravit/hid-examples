{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

import Data.Coerce

newtype Age = Age Int
--  deriving newtype (Eq, Ord)

instance Eq Age where
  (==) = coerce ( (==) :: Int -> Int -> Bool)

instance Ord Age where
  compare = coerce (compare :: Int -> Int -> Ordering)

main :: IO ()
main = do
  print $ Age 42 == Age 42
