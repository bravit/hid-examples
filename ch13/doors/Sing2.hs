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

import Data.Singletons.TH

$(singletons [d|
 data DoorState = Opened | Closed
  deriving Show
 |])

data Door (s :: DoorState) where
  MkDoor :: SingI s => Door s

instance Show (Door s) where
  show MkDoor = show (fromSing (sing :: SDoorState s)) -- Sing s

open :: Door Closed -> Door Opened
open _ = MkDoor

close :: Door Opened -> Door Closed
close _ = MkDoor

data SomeDoor where
  SomeDoor :: Door s -> SomeDoor

deriving instance Show SomeDoor

doorState :: forall s. Door s -> SDoorState s
doorState MkDoor = sing

parseDoor :: String -> Maybe SomeDoor
parseDoor "Opened" = Just $ SomeDoor (MkDoor :: Door Opened)
parseDoor "Closed" = Just $ SomeDoor (MkDoor :: Door Closed)
parseDoor _ = Nothing

changeState :: SomeDoor -> SomeDoor
changeState (SomeDoor d) =
  case doorState d of
    SOpened -> SomeDoor (close d)
    SClosed -> SomeDoor (open d)

test :: String -> IO ()
test d =
  case parseDoor d of
    Just door -> do
        putStrLn $ "Given: " <> show door
        putStrLn $ "Changed: " <> show (changeState door)
    Nothing -> putStrLn "Incorrect argument"

main :: IO ()
main = do
  test "Opened"
  test "Closed"
