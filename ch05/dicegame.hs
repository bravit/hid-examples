import Control.Monad.RWS
import System.Random

type Dice = Int

type DiceGame = RWS (Int, Int) [Dice] StdGen

dice' :: DiceGame Dice
dice' = do
  bs <- ask
  g <- get
  let (r, g') = uniformR bs g
  put g'
  tell [r]
  pure r

dice :: DiceGame Dice
dice = do
  bs <- ask
  r <- state (uniformR bs)
  tell [r]
  pure r

dice'' :: DiceGame Dice
dice'' = ask >>= state . uniformR >>= \r -> tell [r] >> pure r

doubleDice :: DiceGame (Dice, Dice)
doubleDice = (,) <$> dice <*> dice

dices :: Int -> DiceGame [Dice]
dices n = replicateM n dice

diceGame :: DiceGame (Dice, Dice)
diceGame = dice >> dices 5 >> replicateM 2 (dices 3)
                >> dices 10 >> doubleDice

main :: IO ()
main = newStdGen >>= print . evalRWS diceGame (1, 6)
