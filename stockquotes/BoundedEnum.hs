module BoundedEnum (
    BoundedEnum (range)
  ) where

import Prelude (Enum (enumFrom), Bounded (minBound))

class (Enum a, Bounded a) => BoundedEnum a where
  range :: [a]
  range = enumFrom minBound
