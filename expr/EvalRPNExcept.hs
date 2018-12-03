module EvalRPNExcept (evalRPN, EvalError) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Safe

{-
   Function evalRPN evaluates an expression given
   in the reversed polish notation (RPN, postfix notation):

   evalRPN "2 3 +" ==> 5 (== "2 + 3")
   evalRPN "2 3 4 + *" ==> 14 (== 2 * (3 + 4))
-}

data EvalError = NotEnoughElements | NotANumber String | ExtraElements

instance Show EvalError where
  show NotEnoughElements = "Not enough elements in the expression"
  show (NotANumber s) = "Expression component '" <> s <> "' is not a number"
  show ExtraElements = "There are extra elements in the expression"

type Stack = [Integer]

type EvalM = ExceptT EvalError (State Stack)

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  xs <- get
  when (null xs) $ throwError NotEnoughElements
  put (tail xs)
  pure (head xs)

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- gets length
  when (l /= 1) $ throwError ExtraElements

handleNaN :: String -> Maybe Integer -> EvalM Integer
handleNaN s Nothing = throwError (NotANumber s)
handleNaN _ (Just n) = pure n

readSafe :: String -> EvalM Integer
readSafe s = handleNaN s (readMay s)

evalRPN :: String -> Either EvalError Integer
evalRPN str = evalState (runExceptT evalRPN') []
  where
    evalRPN' = traverse_ step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step t   = readSafe t >>= push
    processTops op = op <$> pop <*> pop >>= push

displayResults :: Either EvalError Integer -> String
displayResults (Left e) = "Error: " <> show e
displayResults (Right n) = show n
