{-# LANGUAGE DataKinds #-}

import Elevator.Safe

import Data.Type.Nat
import Control.Monad
import System.Environment

type MX = Nat5

gfElevator :: Elevator MX Nat0 Closed
gfElevator = MkElevator (MkFloor :: Floor MX Nat0)

example :: IO ()
example = pure gfElevator >>= prt
          >>= traceTo (MkFloor :: Floor MX Nat5)
          >>= traceTo (MkFloor :: Floor MX Nat2)
          >>= traceTo (MkFloor :: Floor MX Nat2)
          >>= traceTo (MkFloor :: Floor MX Nat3)
          >>= traceTo (MkFloor :: Floor MX Nat0) >> pure ()
  where
    prt el = print el >> pure el
    traceTo fl el = call fl el >>= prt

simulate :: [SomeFloor MX] -> IO ()
simulate = foldM_ traceToSome (MkSomeElevator gfElevator)
  where
    prt el = print el >> pure el
    traceToSome el fl = callSome fl el >>= prt

main :: IO ()
main = do
    args <- getArgs
    let mbFloors = mapM (mkSomeFloor . read) args
    case mbFloors of
      Nothing -> putStrLn "Incorrect floors"
      Just floors -> simulate floors
