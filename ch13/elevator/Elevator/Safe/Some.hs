{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}


module Elevator.Safe.Some where

import Numeric.Natural
import Data.Type.Nat
import Data.Proxy
import Data.Singletons

import Elevator.Safe.Floor
import Elevator.Safe.Primitive

data SomeFloor (mx :: Nat) where
  MkSomeFloor :: Floor mx cur -> SomeFloor mx

data SomeElevator (mx :: Nat) where
  MkSomeElevator :: SingI door => Elevator mx cur door -> SomeElevator mx

instance Show (SomeElevator mx) where
  show (MkSomeElevator el) = show el

mkSomeFloor :: forall mx. SNatI mx => Natural -> Maybe (SomeFloor mx)
mkSomeFloor cur = reify (fromNatural cur) toSomeFloor
  where
    toSomeFloor :: forall cur. SNatI cur => Proxy cur -> Maybe (SomeFloor mx)
    toSomeFloor _ = MkSomeFloor <$> (mkFloor :: Maybe (Floor mx cur))

