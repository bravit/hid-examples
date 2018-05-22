{-# LANGUAGE
  DeriveAnyClass,
  StandaloneDeriving
#-}

import Data.List (nub, sort)

class (Enum a, Bounded a) => BoundedEnum a where
  range :: [a]
  range = enumFrom minBound

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

data Direction = North | East | South | West
  deriving (Eq, Enum, Bounded, CyclicEnum, Show)

--instance CyclicEnum Direction

data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Enum, Bounded, BoundedEnum)
           
orient :: Turn -> Direction -> Direction
orient TNone = id
orient TLeft = cpred
orient TRight = csucc
orient TAround = cpred . cpred

findTurn :: Direction -> Direction -> Turn
findTurn d1 d2 = head $ filter (\t -> orient t d1 == d2) range

-- standalone deriving
deriving instance BoundedEnum Direction
deriving instance Ord Turn

test :: Bool
test = sort (nub [ findTurn d1 d2 | d1 <- range, d2 <- range ]) == range

main = do
  putStr "Locator test: "
  print test
