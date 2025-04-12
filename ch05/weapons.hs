{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.List (group, sort)
import Control.Monad
import Control.Monad.State
import System.Random
import System.Random.Stateful

data Weapon = Rock | Paper | Scissors
  deriving (Show, Bounded, Enum, Eq)

data Winner = First | Second | Draw
  deriving (Show, Eq, Ord)

winner :: (Weapon, Weapon) -> Winner
winner (Paper, Rock) = First
winner (Scissors, Paper) = First
winner (Rock, Scissors) = First
winner (w1, w2)
  | w1 == w2 = Draw
  | otherwise = Second

instance UniformRange Weapon where
  uniformRM (lo, hi) g = toEnum <$> uniformRM (fromEnum lo, fromEnum hi) g
  isInRange (lo, hi) x = fromEnum x >= fromEnum lo && fromEnum x <= fromEnum hi

instance UniformRange Weapon => Uniform Weapon where
  uniformM rng = uniformRM (minBound, maxBound) rng

randomWeapon :: State StdGen Weapon
randomWeapon = state uniform

gameRound :: State StdGen (Weapon, Weapon)
gameRound = (,) <$> randomWeapon <*> randomWeapon

game :: Int -> State StdGen [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound)
  where
    counts xs = map headLength $ group $ sort xs
    headLength xs@(x:_) = (x, length xs)
    headLength [] = error "unexpected"

main :: IO ()
main = do
  g <- newStdGen
  let r = evalState (game 10) g
  print r
