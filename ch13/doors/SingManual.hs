{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

data DoorState = Opened | Closed
  deriving Show

data SDoorState (s :: DoorState) where
  SClosed :: SDoorState Closed
  SOpened :: SDoorState Opened

class SDoorStateI (s :: DoorState) where
  sDoorState :: SDoorState s

instance SDoorStateI Opened where
  sDoorState = SOpened

instance SDoorStateI Closed where
  sDoorState = SClosed

data Door (s :: DoorState) where
  MkDoor :: SDoorStateI s => Door s

doorState :: forall s. Door s -> DoorState
doorState MkDoor =
  case sDoorState :: SDoorState s of
    SOpened -> Opened
    SClosed -> Closed

instance Show (Door s) where
  show d = "Door " <> show (doorState d)

open :: Door Closed -> Door Opened
open _ = MkDoor

close :: Door Opened -> Door Closed
close _ = MkDoor

data SomeDoor where
  SomeDoor :: Door s -> SomeDoor

deriving instance Show SomeDoor

parseDoor :: String -> Maybe SomeDoor
parseDoor "Opened" = Just $ SomeDoor (MkDoor :: Door Opened)
parseDoor "Closed" = Just $ SomeDoor (MkDoor :: Door Closed)
parseDoor _ = Nothing

switchState :: forall s. Door s -> SomeDoor
switchState door@MkDoor =
  case sDoorState :: SDoorState s of
    SOpened -> SomeDoor (close door)
    SClosed -> SomeDoor (open door)

switchSome :: SomeDoor -> SomeDoor
switchSome (SomeDoor d) = switchState d

test :: String -> IO ()
test d =
  case parseDoor d of
    Just door -> do
        putStrLn $ "Given: " <> show door
        putStrLn $ "Switched: " <> show (switchSome door)
    Nothing -> putStrLn "Incorrect argument"

main :: IO ()
main = do
  test "Opened"
  test "Closed"
