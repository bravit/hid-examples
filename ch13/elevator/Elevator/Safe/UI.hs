{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Elevator.Safe.UI (call, callSome) where

import Control.Monad.Trans
import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Type.Dec
import Data.Type.Equality
import Data.Void

import Elevator.Safe.Primitive
import Elevator.Safe.Floor

data ElevatorMove mx to from where
  StandStill :: ElevatorMove mx to to
  GoingUp :: BelowTop mx from => ElevatorMove mx to from
  GoingDown :: from ~ S fl => ElevatorMove mx to from

decideMove :: forall mx to from.
  Floor mx to -> Elevator mx from Closed -> ElevatorMove mx to from
decideMove MkFloor MkElevatorClosed =
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

moveTo :: forall mx to from door m. MonadIO m =>
          Floor mx to -> Elevator mx from door ->
          m (Elevator mx to Closed)
moveTo fl el' = do
  el <- ensureClosed el'
  case decideMove fl el :: ElevatorMove mx to from of
    StandStill -> pure el
    GoingUp -> up el >>= moveTo fl
    GoingDown -> down el >>= moveTo fl

call :: MonadIO m =>
  Floor mx to -> Elevator mx from door -> m (Elevator mx to Opened)
call fl el = do
  liftIO $ putStrLn $ "Call to: " <> show fl
  case sameFloor fl el of
    Just Refl -> ensureOpenedAt fl el
    Nothing -> moveTo fl el >>= open fl

callSome :: MonadIO m =>
  SomeFloor mx -> SomeElevator mx -> m (SomeElevator mx)
callSome (MkSomeFloor fl) (MkSomeElevator el) =
  MkSomeElevator <$> call fl el
