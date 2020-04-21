{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Function ((&))
import Control.Monad (foldM)
import Text.Read (readMaybe)

data DoorState = Opened | Closed
  deriving Show

data SDoorState (s :: DoorState) where
  SClosed :: SDoorState Closed
  SOpened :: SDoorState Opened

deriving instance Show (SDoorState s)

class SDoorStateI (s :: DoorState) where
  sState :: SDoorState s

instance SDoorStateI Opened where
  sState = SOpened

instance SDoorStateI Closed where
  sState = SClosed

data Door (s :: DoorState) where
  MkDoor :: Door Closed
  Open :: Door Closed -> Door Opened
  Close :: Door Opened -> Door Closed
  Hold :: Door s -> Door s

deriving instance Show (Door s)

openedDoor = Open MkDoor

-- badDoor = Close MkDoor

someDoor = MkDoor & Open & Hold & Hold & Close & Hold & Open 
-- (&) = flip ($) -- imported from Data.Function (base)

data AnyDoor where
  AnyDoor :: SDoorStateI s => Door s -> AnyDoor

deriving instance Show AnyDoor

sDoorState :: SDoorStateI s => Door s -> SDoorState s
sDoorState _ = sState

applyCmd :: AnyDoor -> String -> Maybe AnyDoor
applyCmd (AnyDoor d) cmd =
  case (sDoorState d, cmd) of
    (SClosed, "Open") -> Just $ AnyDoor $ Open d
    (SOpened, "Close") -> Just $ AnyDoor $ Close d
    (_, "Hold") -> Just $ AnyDoor $ Hold d
    _ -> Nothing

parseDoor :: String -> Maybe AnyDoor
parseDoor =
  foldM applyCmd (AnyDoor MkDoor) . words

testDoor :: Maybe AnyDoor
testDoor = parseDoor "Open Hold Hold Close Hold Open Hold Close Open"

-- Exercise

extractDoor :: AnyDoor ->
               Either (Door Closed) (Door Opened)
extractDoor = undefined

-- The best tutorial on the singletons library:
-- https://blog.jle.im/entries/series/+introduction-to-singletons.html

main = print testDoor
