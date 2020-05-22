import Data.List (group, sort)
import Control.Monad (replicateM)
import Control.Monad.State
import System.Random

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

instance Random Weapon where
    randomR (a,b) g =
      case randomR (fromEnum a, fromEnum b) g of
        (r, g') -> (toEnum r, g')
    random g = randomR (minBound, maxBound) g

randomWeapon :: State StdGen Weapon
randomWeapon = state random

gameRound :: State StdGen (Weapon, Weapon)
gameRound = (,) <$> randomWeapon <*> randomWeapon

game :: Int -> State StdGen [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound)
  where
    counts xs = map headLength $ group $ sort xs
    headLength xs@(x:_) = (x, length xs)

main = do
  g <- newStdGen
  let r = evalState (game 10) g
  print r
