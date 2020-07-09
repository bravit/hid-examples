{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Elevator.Safe.Primitive where

import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Singletons.TH
import Control.Monad.Trans

import qualified Elevator.LowLevel as LL
import Elevator.Safe.Floor

$(singletons [d|
 data DoorState = Opened | Closed
  deriving (Eq, Show)
  |])

data Elevator (mx :: Nat) (cur :: Nat) (door :: DoorState) where
  MkElevatorClosed :: GoodFloor mx cur => Elevator mx cur Closed
  MkElevatorOpened :: GoodFloor mx cur => Elevator mx cur Opened

instance SingI door => Show (Elevator mx cur door) where
  show it@MkElevatorClosed = showElev it
  show it@MkElevatorOpened = showElev it

showElev :: forall mx cur door. (GoodFloor mx cur, SingI door) =>
            Elevator mx cur door -> String
showElev _ = "Elevator {current = "
             <> show (MkFloor :: Floor mx cur)
             <> ", door = "
             <> show (fromSing (sing :: Sing door))
             <> "}"

data SomeElevator (mx :: Nat) where
  MkSomeElevator :: SingI door => Elevator mx cur door -> SomeElevator mx

instance Show (SomeElevator mx) where
  show (MkSomeElevator el) = show el

up :: (BelowTop mx cur, MonadIO m) =>
      Elevator mx cur Closed -> m (Elevator mx (S cur) Closed)
up _ = do
  LL.up
  pure MkElevatorClosed

down :: forall mx cur m. MonadIO m =>
        Elevator mx (S cur) Closed -> m (Elevator mx cur Closed)
down MkElevatorClosed = do
  LL.down
  pure $ withSNat (snatPrev (snat :: SNat (S cur))) $
           withLEProof (leStepL (leProof :: LEProof (S cur) mx))
             MkElevatorClosed
 where
   snatPrev :: forall n. SNat (S n) -> SNat n
   snatPrev sn@SS = withSNat sn snat

open :: MonadIO m =>
        Floor mx cur -> Elevator mx cur Closed -> m (Elevator mx cur Opened)
open _ MkElevatorClosed = do
  LL.open
  pure MkElevatorOpened

close :: MonadIO m =>
         Floor mx cur -> Elevator mx cur Opened -> m (Elevator mx cur Closed)
close _ MkElevatorOpened = do
  LL.close
  pure MkElevatorClosed

sameFloor :: forall mx to from door.
             Floor mx to -> Elevator mx from door -> Maybe (to :~: from)
sameFloor MkFloor MkElevatorClosed = eqNat
sameFloor MkFloor MkElevatorOpened = eqNat

ensureClosed :: forall mx cur door m. MonadIO m =>
  Elevator mx cur door -> m (Elevator mx cur Closed)
ensureClosed el@MkElevatorClosed = pure el
ensureClosed el@MkElevatorOpened = close (MkFloor :: Floor mx cur) el

ensureOpenedAt :: MonadIO m =>
  Floor mx fl -> Elevator mx fl door -> m (Elevator mx fl Opened)
ensureOpenedAt _ el@MkElevatorOpened = pure el
ensureOpenedAt fl el@MkElevatorClosed = open fl el
