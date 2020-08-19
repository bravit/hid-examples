module Elevator.Unsafe where

import qualified Elevator.LowLevel as LL

import Control.Monad.Trans

data DoorState = Opened | Closed
  deriving (Eq, Show)

newtype Floor = Floor Int
  deriving (Eq, Ord)

instance Bounded Floor where
  minBound = Floor 0
  maxBound = Floor 5

instance Show Floor where
  show (Floor fl) = "Floor " <> show fl <> " of " <> show mx
    where
      Floor mx = maxBound

data Elevator = Elevator {
     current :: Floor,
     door :: DoorState
  }
  deriving Show

sameFloor :: Floor -> Elevator -> Bool
sameFloor fl el = fl == current el

isClosed :: Elevator -> Bool
isClosed el = door el == Closed

isOpened :: Elevator -> Bool
isOpened el = door el == Opened

belowTop :: Floor -> Bool
belowTop fl = fl < maxBound

aboveGround :: Floor -> Bool
aboveGround fl = fl > minBound

down :: MonadIO m => Elevator -> m Elevator
down el@(Elevator fl@(Floor n) Closed)
  | aboveGround fl = do
      liftIO $ LL.down
      pure $ el {current = Floor (n - 1)}
  | otherwise = error "Elevator is on the ground floor"
down (Elevator _ Opened) = error "Door must be closed before move"

up :: MonadIO m => Elevator -> m Elevator
up el@(Elevator fl@(Floor n) Closed)
  | belowTop fl = do
      liftIO $ LL.up
      pure $ el {current = Floor (n + 1)}
  | otherwise = error "Elevator on the top floor"
up (Elevator _ Opened) = error "Door must be closed before move"

open :: MonadIO m => Floor -> Elevator -> m Elevator
open fl el
  | sameFloor fl el =
      if isClosed el
      then do
        liftIO $ LL.open
        pure $ el {door = Opened}
      else error "Door is already opened"
  | otherwise = error "Can't operate the door with an elevator elsewhere"

close :: MonadIO m => Floor -> Elevator -> m Elevator
close fl el
  | sameFloor fl el =
      if isOpened el
      then do
        liftIO $ LL.close
        pure $ el {door = Closed}
      else error "Door is already closed"
  | otherwise = error "Can't operate the door with an elevator elsewhere"

ensureClosed :: MonadIO m => Elevator -> m Elevator
ensureClosed el
  | isClosed el = pure el
  | otherwise = close (current el) el

moveTo :: MonadIO m => Floor -> Elevator -> m Elevator
moveTo fl el' = do
  el <- ensureClosed el'
  case compare fl (current el) of
    EQ -> pure el
    GT -> up el >>= moveTo fl
    LT -> down el >>= moveTo fl

call :: MonadIO m => Floor -> Elevator -> m Elevator
call fl el = do
  liftIO $ putStrLn $ "Call to: " <> show fl
  if sameFloor fl el
    then (if isOpened el then pure el else open fl el)
    else (moveTo fl el >>= open fl)
