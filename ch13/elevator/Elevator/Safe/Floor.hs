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
import Data.Type.Dec
import Data.Type.Equality
import Data.Void

type GoodFloor mx cur = (SNatI mx, SNatI cur, LE cur mx)
type BelowTop mx cur = (SNatI mx, SNatI cur, LE (S cur) mx)

data Floor (mx :: Nat) (cur :: Nat) where
  MkFloor :: GoodFloor mx cur => Floor mx cur

instance Show (Floor mx cur) where
  show MkFloor = "Floor " <> show (snatToNat (snat :: SNat cur))
                 <> " of " <> show (snatToNat (snat :: SNat mx))

next :: BelowTop mx cur => Floor mx cur -> Floor mx (S cur)
next _ = MkFloor

prev :: forall mx cur. Floor mx (S cur) -> Floor mx cur
prev MkFloor =
        withSNat (snatPrev (snat :: SNat (S cur))) $
           withLEProof (leStepL (leProof :: LEProof (S cur) mx))
             MkFloor
 where
   snatPrev :: forall n. SNat (S n) -> SNat n
   snatPrev sn@SS = withSNat sn snat

sameFloor :: forall mx to from.
             Floor mx to -> Floor mx from -> Maybe (to :~: from)
sameFloor MkFloor MkFloor = eqNat

mkFloor :: forall mx cur. (SNatI mx, SNatI cur) => Maybe (Floor mx cur)
mkFloor =
  case decideLE :: Dec (LEProof cur mx) of
    Yes prf -> withLEProof prf $ Just MkFloor
    No _ -> Nothing

data Move mx to from where
  StandStill :: Move mx to to
  GoingUp :: BelowTop mx from => Move mx to from
  GoingDown :: from ~ S fl => Move mx to from

decideMove :: forall mx to from.
  Floor mx to -> Floor mx from -> Move mx to from
decideMove MkFloor MkFloor =
  case discreteNat :: Dec (to :~: from) of
    Yes Refl -> StandStill -- to == from
    No to_neq_from ->
      case decideLE :: Dec (LEProof to from) of
        Yes to_le_from -> -- to < from
           withAboveGround to_le_from to_neq_from GoingDown
        No to_gt_from -> -- to > from
           withLEProof (belowTop to_gt_from) GoingUp
  where
    belowTop :: LE to mx => Neg (LEProof to from) -> LEProof (S from) mx
    belowTop neg = leTrans (leSwap neg) leProof

    withAboveGround :: LEProof to from -> Neg (to :~: from) ->
                       (forall fl. from ~ S fl => r) -> r
    withAboveGround LEZero neq r =
      case snat :: SNat from of
        SZ -> absurd $ neq Refl
        SS -> r
    withAboveGround (LESucc _) _ r = r
