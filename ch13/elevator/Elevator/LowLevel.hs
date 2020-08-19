module Elevator.LowLevel where

up :: IO ()
up = putStrLn "Going up"

down :: IO ()
down = putStrLn "Going down"

open :: IO ()
open = putStrLn "Door is opening"

close :: IO ()
close = putStrLn "Door is closing"
