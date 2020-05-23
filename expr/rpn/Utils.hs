module Utils where

import Control.Applicative
import Text.Read (readMaybe)

guardMaybe :: Alternative m => Maybe a -> m a
guardMaybe Nothing = empty
guardMaybe (Just a) = pure a

readSafe :: (Read a, Alternative m) => String -> m a
readSafe s = guardMaybe $ readMaybe s
