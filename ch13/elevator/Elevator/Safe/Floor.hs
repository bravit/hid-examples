{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Elevator.Safe.Floor where

import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Type.Equality
import Data.Type.Dec


type GoodFloor mx cur = (SNatI mx, SNatI cur, LE cur mx)
type BelowTop mx cur = LE (S cur) mx

data Floor (mx :: Nat) (cur :: Nat) where
  MkFloor :: GoodFloor mx cur => Floor mx cur

instance Show (Floor mx cur) where
  show MkFloor = "Floor " <> show (snatToNat (snat :: SNat cur))
                 <> " of " <> show (snatToNat (snat :: SNat mx))

next :: BelowTop mx cur => Floor mx cur -> Floor mx (S cur)
next MkFloor = MkFloor

prev :: forall mx cur. Floor mx (S cur) -> Floor mx cur
prev MkFloor =
  withSNat snatCur $
    withLEProof leCur
      MkFloor
 where
   snatCur :: SNat cur
   snatCur = case snat :: SNat (S cur) of
               SS -> snat
   leCur :: LEProof cur mx
   leCur = leStepL leProof

sameFloor :: forall mx to from.
             Floor mx to -> Floor mx from -> Maybe (to :~: from)
sameFloor MkFloor MkFloor = eqNat

mkFloor :: forall mx cur. (SNatI mx, SNatI cur) => Maybe (Floor mx cur)
mkFloor =
  case decideLE :: Dec (LEProof cur mx) of
    Yes prf -> withLEProof prf $ Just MkFloor
    No _ -> Nothing
