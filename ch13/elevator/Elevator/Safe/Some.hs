{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Elevator.Safe.Some where

import Numeric.Natural
import Data.Type.Nat
import Data.Proxy

import Elevator.Safe.Floor
import Elevator.Safe.Primitive

data SomeFloor (mx :: Nat) where
  MkSomeFloor :: Floor mx cur -> SomeFloor mx

data SomeElevator (mx :: Nat) where
  MkSomeElevator :: Elevator mx cur door -> SomeElevator mx

instance Show (SomeElevator mx) where
  show (MkSomeElevator el) = show el

mkSomeFloor :: forall mx. SNatI mx => Natural -> Maybe (SomeFloor mx)
mkSomeFloor cur = reify (fromNatural cur) (fmap MkSomeFloor . toMbFloor)
  where
    toMbFloor :: SNatI cur => Proxy cur -> Maybe (Floor mx cur)
    toMbFloor _ = mkFloor

