module EvalRPN_trans2 (evalRPN) where

import Control.Monad.State
--import Control.Monad.Trans.Maybe
import MyMaybeT
import Safe

{-
   Function evalRPN evaluates an expression given
   in the reversed polish notation (RPN, postfix notation):

   evalRPN "2 3 +" ==> 5 (== "2 + 3")
   evalRPN "2 3 4 + *" ==> 14 (== 2 * (3 + 4))
-}

type Stack = [Integer]

type EvalM = MaybeT (State Stack)

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  xs <- get
  guard (not $ null xs)
  put (tail xs)
  pure (head xs)

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- length <$> get
  guard (l == 1)

readSafe :: String -> EvalM Integer
readSafe s = MaybeT . pure $ readMay s

evalRPN :: String -> Maybe Integer
evalRPN str = evalState (runMaybeT evalRPN') []
  where
    evalRPN' = traverse step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step t   = readSafe t >>= push
    processTops op = op <$> pop <*> pop >>= push
