{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elevator.Safe (module X, call, callSome, SomeFloor(..),
                     SomeElevator(..), mkSomeFloor) where

import Control.Monad.Trans
import Data.Type.Equality
import Numeric.Natural
import Data.Type.Nat
import Data.Proxy

import Elevator.Safe.Floor as X
import Elevator.Safe.Operations as X
import Elevator.Safe.Moves as X

data SomeFloor (mx :: Nat) where
  MkSomeFloor :: Floor mx cur -> SomeFloor mx

data SomeElevator (mx :: Nat) where
  MkSomeElevator :: Elevator mx cur door -> SomeElevator mx

instance Show (SomeElevator mx) where
  show (MkSomeElevator el) = show el

moveTo :: MonadIO m =>
          Floor mx to -> Elevator mx from Closed -> m (Elevator mx to Closed)
moveTo fl el =
  case decideMove fl (currentFloor el) of
    StandStill -> pure el
    GoingUp -> up el >>= moveTo fl
    GoingDown -> down el >>= moveTo fl

call :: MonadIO m =>
        Floor mx to -> Elevator mx from door -> m (Elevator mx to Opened)
call fl el = do
  liftIO $ putStrLn $ "Call to: " <> show fl
  case sameFloor fl (currentFloor el) of
    Just Refl -> ensureOpenedAt fl el
    Nothing -> ensureClosed el >>= moveTo fl >>= open fl

mkSomeFloor :: forall mx. SNatI mx => Natural -> Maybe (SomeFloor mx)
mkSomeFloor cur = reify (fromNatural cur) (fmap MkSomeFloor . toMbFloor)
  where
    toMbFloor :: SNatI fl => Proxy fl -> Maybe (Floor mx fl)
    toMbFloor _ = mkFloor

callSome :: MonadIO m =>
            SomeFloor mx -> SomeElevator mx -> m (SomeElevator mx)
callSome (MkSomeFloor fl) (MkSomeElevator el) =
  MkSomeElevator <$> call fl el
