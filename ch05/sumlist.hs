import Data.Foldable
import Control.Monad.State

addItem :: Integer -> State Integer ()
addItem n = do
  s <- get
  put (s + n)

addItem' :: Integer -> State Integer ()
addItem' n = modify' (+n)

sumList :: [Integer] -> State Integer ()
sumList xs = traverse_ addItem xs

answer :: Integer
answer = execState (sumList [1..100]) 0

main :: IO ()
main = print answer
