{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Elevator.Safe (module X, call, callSome) where

import Control.Monad.Trans
import Data.Type.Equality

import Elevator.Safe.Floor as X
import Elevator.Safe.Primitive as X
import Elevator.Safe.Some as X

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

callSome :: MonadIO m =>
            SomeFloor mx -> SomeElevator mx -> m (SomeElevator mx)
callSome (MkSomeFloor fl) (MkSomeElevator el) =
  MkSomeElevator <$> call fl el
