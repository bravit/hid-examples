module EvalRPN_trans (evalRPN) where

import Control.Monad.State

import Safe

{-
   Function evalRPN evaluates an expression given
   in the reversed polish notation (RPN, postfix notation):

   evalRPN "2 3 +" ==> 5 (== "2 + 3")
   evalRPN "2 3 4 + *" ==> 14 (== 2 * (3 + 4))
-}

type Stack = [Integer]

type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  xs <- get
  guard (not $ null xs)
  put (tail xs)
  pure (head xs)
--  lift (headMay xs)
  

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- length <$> get
  guard (l == 1)
--  when (l /= 1) $ lift Nothing

readSafe :: String -> EvalM Integer
readSafe s = lift (readMay s)

evalRPN :: String -> Maybe Integer
evalRPN str = evalStateT evalRPN' []
  where
    evalRPN' = traverse step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step t   = readSafe t >>= push
    processTops op = op <$> pop <*> pop >>= push
