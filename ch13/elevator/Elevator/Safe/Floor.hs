{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Elevator.Safe.Floor where

import Numeric.Natural
import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Type.Dec
import Data.Proxy

type GoodFloor mx cur = (SNatI mx, SNatI cur, LE cur mx)
type BelowTop mx cur = (SNatI mx, SNatI cur, LE (S cur) mx)

data Floor (mx :: Nat) (cur :: Nat) where
  MkFloor :: GoodFloor mx cur => Floor mx cur

data SomeFloor (mx :: Nat) where
  MkSomeFloor :: Floor mx cur -> SomeFloor mx

instance Show (Floor mx cur) where
  show MkFloor = "Floor " <> show (snatToNat (snat :: SNat cur))
                 <> " of " <> show (snatToNat (snat :: SNat mx))

mkFloor :: forall mx cur. (SNatI mx, SNatI cur) => Maybe (Floor mx cur)
mkFloor =
  case decideLE :: Dec (LEProof cur mx) of
    Yes prf -> withLEProof prf $ Just MkFloor
    No _ -> Nothing

mkSomeFloor :: forall mx. SNatI mx => Natural -> Maybe (SomeFloor mx)
mkSomeFloor cur = reify (fromNatural cur) toSomeFloor
  where
    toSomeFloor :: forall cur. SNatI cur => Proxy cur -> Maybe (SomeFloor mx)
    toSomeFloor _ = MkSomeFloor <$> (mkFloor :: Maybe (Floor mx cur))
