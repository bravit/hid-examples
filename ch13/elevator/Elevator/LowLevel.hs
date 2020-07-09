module Elevator.LowLevel where

import Control.Monad.Trans

up :: MonadIO m => m ()
up = liftIO $ putStrLn "Going up"

down :: MonadIO m => m ()
down = liftIO $ putStrLn "Going down"

open :: MonadIO m => m ()
open = liftIO $ putStrLn "Door is opening"

close :: MonadIO m => m ()
close = liftIO $ putStrLn "Door is closing"
