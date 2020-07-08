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

module Safe.Elevator where

import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Type.Dec
import Data.Singletons.TH
import Control.Monad.Trans

import qualified LowLevel.Ops as LL

$(singletons [d|
 data DoorState = Opened | Closed
  deriving (Eq, Show)
  |])

data Floor (mx :: Nat) (num :: Nat) where
  MkFloor :: GoodFloor mx num => Floor mx num

instance Show (Floor mx num) where
  show MkFloor = "Floor " <> show (snatToNat (snat :: SNat num))
                 <> " of " <> show (snatToNat (snat :: SNat mx))


mkFloor :: forall mx num. (SNatI mx, SNatI num) => Maybe (Floor mx num)
mkFloor =
  case decideLE :: Dec (LEProof num mx) of
    Yes prf -> withLEProof prf $ Just MkFloor
    No _ -> Nothing


type GoodFloor mx num = (SNatI mx, SNatI num, LE num mx)
type BelowTop mx num = (SNatI mx, SNatI num, LE (S num) mx)

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
