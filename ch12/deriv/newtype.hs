{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Coerce
import Control.Monad.State

newtype Age = Age Int
--  deriving newtype (Eq, Ord)

instance Eq Age where
  (==) = coerce ((==) :: Int -> Int -> Bool)

instance Ord Age where
  compare = coerce (compare :: Int -> Int -> Ordering)

newtype MyApp a = MyApp {
    runApp :: StateT Int IO a
  }
--  deriving (Functor, Applicative, Monad)

instance Functor MyApp where
  fmap :: forall a b. (a -> b) -> MyApp a -> MyApp b
  fmap = coerce (fmap :: (a -> b) -> StateT Int IO a -> StateT Int IO b)

type family Inspect t
type instance Inspect Int = Bool
type instance Inspect Age = Int

class Inspector a where
  inspect :: a -> Inspect a

instance Inspector Int where
  inspect n = n > 0

-- doesn't compile:
-- deriving newtype instance Inspector Age

main :: IO ()
main = do
  print $ Age 42 < Age 42

