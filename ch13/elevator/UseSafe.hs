{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

import Data.Proxy
import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Type.Equality
import Data.Type.Dec
import Data.Singletons.TH
import Control.Monad.Trans
import Data.Void
import Numeric.Natural
import System.Environment
import Control.Monad

import Safe.Elevator


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

ensureClosed :: forall mx cur door m. MonadIO m => Elevator mx cur door -> m (Elevator mx cur Closed)
ensureClosed el@MkElevatorClosed = pure el
ensureClosed el@MkElevatorOpened = close (MkFloor :: Floor mx cur) el

moveTo :: forall mx to from door m. MonadIO m =>
          Floor mx to -> Elevator mx from door ->
          m (Elevator mx to Closed)
moveTo fl el' = do
  el <- ensureClosed el'
  case decideMove fl el :: ElevatorMove mx to from of
    StandStill -> pure el
    GoingUp -> up el >>= moveTo fl
    GoingDown -> down el >>= moveTo fl

sameFloor :: forall mx to from door.
             Floor mx to -> Elevator mx from door -> Maybe (to :~: from)
sameFloor MkFloor MkElevatorClosed = eqNat
sameFloor MkFloor MkElevatorOpened = eqNat

ensureOpenedAt :: MonadIO m =>
  Floor mx fl -> Elevator mx fl door -> m (Elevator mx fl Opened)
ensureOpenedAt _ el@MkElevatorOpened = pure el
ensureOpenedAt fl el@MkElevatorClosed = open fl el

call :: MonadIO m =>
  Floor mx to -> Elevator mx from door -> m (Elevator mx to Opened)
call fl el = do
  liftIO $ putStrLn $ "Call to: " <> show fl
  case sameFloor fl el of
    Just Refl -> ensureOpenedAt fl el
    Nothing -> moveTo fl el >>= open fl

type MX = Nat5

gfElevator :: Elevator MX Nat0 Closed
gfElevator = MkElevatorClosed

example :: IO ()
example = pure gfElevator >>= prt
          >>= traceTo (MkFloor :: Floor MX Nat5)
          >>= traceTo (MkFloor :: Floor MX Nat2)
          >>= traceTo (MkFloor :: Floor MX Nat2)
          >>= traceTo (MkFloor :: Floor MX Nat3)
          >>= traceTo (MkFloor :: Floor MX Nat0) >> pure ()
  where
    prt el = print el >> pure el
    traceTo fl el = call fl el >>= prt

data SomeFloor (mx :: Nat) where
  MkSomeFloor :: Floor mx num -> SomeFloor mx

mkSomeFloor :: forall mx. SNatI mx => Natural -> Maybe (SomeFloor mx)
mkSomeFloor num = reify (fromNatural num) toSomeFloor
  where
    toSomeFloor :: forall num. SNatI num => Proxy num -> Maybe (SomeFloor mx)
    toSomeFloor _ = MkSomeFloor <$> (mkFloor :: Maybe (Floor mx num))

data SomeElevator (mx :: Nat) where
  MkSomeElevator :: SingI door => Elevator mx cur door -> SomeElevator mx

instance Show (SomeElevator mx) where
  show (MkSomeElevator el) = show el

callSome :: MonadIO m =>
  SomeFloor MX -> SomeElevator MX -> m (SomeElevator MX)
callSome (MkSomeFloor fl) (MkSomeElevator el) =
  MkSomeElevator <$> call fl el

simulate :: [SomeFloor MX] -> IO ()
simulate = foldM_ traceToSome (MkSomeElevator gfElevator)
  where
    prt el = print el >> pure el
    traceToSome el fl = callSome fl el >>= prt

main :: IO ()
main = do
    args <- getArgs
    let mbFloors = mapM (mkSomeFloor . read) args
    case mbFloors of
      Nothing -> putStrLn "Incorrect floors"
      Just floors -> simulate floors
