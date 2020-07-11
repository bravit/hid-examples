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
{-# LANGUAGE StandaloneDeriving #-}

module Elevator.Safe.Primitive where

import Data.Type.Nat
import Data.Singletons.TH
import Control.Monad.Trans

import qualified Elevator.LowLevel as LL
import Elevator.Safe.Floor

$(singletons [d|
 data Door = Opened | Closed
  deriving (Eq, Show)
  |])

data Elevator (mx :: Nat) (cur :: Nat) (door :: Door) where
  MkElevatorClosed :: Floor mx cur -> Elevator mx cur Closed
  MkElevatorOpened :: Floor mx cur -> Elevator mx cur Opened

instance SingI door => Show (Elevator mx cur door) where
  show it@(MkElevatorClosed MkFloor) = showElev it
  show it@(MkElevatorOpened MkFloor) = showElev it

showElev :: forall mx cur door. (GoodFloor mx cur, SingI door) =>
            Elevator mx cur door -> String
showElev _ = "Elevator {current = "
             <> show (MkFloor :: Floor mx cur)
             <> ", door = "
             <> show (fromSing (sing :: Sing door))
             <> "}"

currentFloor :: Elevator mx cur door -> Floor mx cur
currentFloor (MkElevatorClosed fl) = fl
currentFloor (MkElevatorOpened fl) = fl

up :: (BelowTop mx cur, MonadIO m) =>
      Elevator mx cur Closed -> m (Elevator mx (S cur) Closed)
up (MkElevatorClosed fl) = do
  LL.up
  pure (MkElevatorClosed $ next fl)

down :: MonadIO m => Elevator mx (S cur) Closed -> m (Elevator mx cur Closed)
down (MkElevatorClosed fl) = do
  LL.down
  pure $ MkElevatorClosed $ prev fl

open :: MonadIO m =>
        Floor mx cur -> Elevator mx cur Closed -> m (Elevator mx cur Opened)
open _ (MkElevatorClosed fl) = do
  LL.open
  pure (MkElevatorOpened fl)

close :: MonadIO m =>
         Floor mx cur -> Elevator mx cur Opened -> m (Elevator mx cur Closed)
close _ (MkElevatorOpened fl) = do
  LL.close
  pure (MkElevatorClosed fl)

ensureClosed :: MonadIO m => Elevator mx cur door -> m (Elevator mx cur Closed)
ensureClosed el@(MkElevatorClosed _) = pure el
ensureClosed el@(MkElevatorOpened fl) = close fl el

ensureOpenedAt :: MonadIO m =>
  Floor mx fl -> Elevator mx fl door -> m (Elevator mx fl Opened)
ensureOpenedAt _ el@(MkElevatorOpened _) = pure el
ensureOpenedAt fl el@(MkElevatorClosed _) = open fl el
