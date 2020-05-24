{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}


import Data.Function ((&))
import Control.Monad (foldM)
import Data.Type.Equality ((:~:) (Refl))


data DoorState = Opened | Closed
  deriving Show

data Door (s :: DoorState) where
  MkDoor :: Door 'Closed
  Open :: Door 'Closed -> Door 'Opened
  Close :: Door 'Opened -> Door 'Closed
  Hold :: Door s -> Door s

deriving instance Show (Door s)

openedDoor :: Door 'Opened
openedDoor = Open MkDoor

-- badDoor = Close MkDoor

someDoor :: Door 'Opened
someDoor = MkDoor & Open & Hold & Hold & Close & Hold & Open
-- (&) = flip ($) -- imported from Data.Function (base)

parseDoorAttempt :: String -> Door s
parseDoorAttempt _ = undefined

data AnyDoor where
  AnyDoor :: Door s -> AnyDoor

deriving instance Show AnyDoor

applyCmdAttempt :: AnyDoor -> String ->
                   Maybe AnyDoor
applyCmdAttempt (AnyDoor _) "Open" = undefined -- Just $ AnyDoor $ Open d
--applyCmdAttempt (AnyDoor d) "Open" = Just $ AnyDoor $ Open d
--applyCmdAttempt (AnyDoor d) "Close" = Just $ AnyDoor $ Close d
applyCmdAttempt _ _ = Nothing

doorState :: Door s -> DoorState
doorState MkDoor = Closed
doorState (Open _) = Opened
doorState (Close _) = Closed
doorState (Hold d) = doorState d

{-
applyCmdAttempt2 :: AnyDoor -> String -> Maybe AnyDoor
applyCmdAttempt2 (AnyDoor d) "Open" =
  case doorState d of
    Closed -> Just $ AnyDoor $ Open d
    _ -> Nothing
-}

checkClosed :: Door s -> Maybe (s :~: 'Closed)
checkClosed MkDoor = Just Refl
checkClosed (Close _) = Just Refl
checkClosed (Hold d) = checkClosed d
checkClosed _ = Nothing

checkOpened :: Door s -> Maybe (s :~: 'Opened)
checkOpened (Open _) = Just Refl
checkOpened (Hold d) = checkOpened d
checkOpened _ = Nothing

justAny :: Door s -> Maybe AnyDoor
justAny = Just . AnyDoor

applyCmd :: AnyDoor -> String -> Maybe AnyDoor
applyCmd (AnyDoor d) "Open" =
  case checkClosed d of
    Just Refl -> justAny $ Open d -- !!!
                 -- Refl :: s :~: Closed
    Nothing -> Nothing
applyCmd (AnyDoor d) "Close" =
  checkOpened d >>= \Refl -> justAny $ Close d
applyCmd (AnyDoor d) "Hold" = justAny $ Hold d
applyCmd _ _ = Nothing

parseDoor :: String -> Maybe AnyDoor
parseDoor =
  foldM applyCmd (AnyDoor MkDoor) . words

testDoor :: Maybe AnyDoor
testDoor = parseDoor "Open Hold Hold Close Hold Open Hold Close Open"

anyDoorState :: AnyDoor -> DoorState
anyDoorState = undefined

extractDoor :: AnyDoor ->
               Either (Door 'Closed) (Door 'Opened)
extractDoor = undefined

extractClosed :: AnyDoor -> Maybe (Door 'Closed)
extractClosed = undefined

extractOpened :: AnyDoor -> Maybe (Door 'Opened)
extractOpened = undefined

main :: IO ()
main = print testDoor
