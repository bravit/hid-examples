{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif


import Data.Singletons.TH

$(singletons [d|
 data DoorState = Opened | Closed
  deriving Show
 |])

data Door (s :: DoorState) where
  MkDoor :: SingI s => Door s

doorState :: forall s. Door s -> DoorState
doorState MkDoor = fromSing (sing :: SDoorState s)

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
  case sing :: SDoorState s of
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
