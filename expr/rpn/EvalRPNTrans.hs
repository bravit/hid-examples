module EvalRPNTrans (evalRPN) where

import Control.Monad.State
import Utils

type Stack = [Integer]
type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  (x:xs) <- get
  put xs
  pure x

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- length <$> get
  guard (l == 1)
--  when (l /= 1) $ lift Nothing

evalRPN :: String -> Maybe Integer
evalRPN str = evalStateT evalRPN' []
  where
    evalRPN' = traverse step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t   = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push
