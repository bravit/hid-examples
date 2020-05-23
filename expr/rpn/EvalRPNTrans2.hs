module EvalRPNTrans2 (evalRPN) where

import Control.Monad.State
import Utils
import MyMaybeT

type Stack = [Integer]
type EvalM = MaybeT (State Stack)

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

evalRPN :: String -> Maybe Integer
evalRPN str = evalState (runMaybeT evalRPN') []
  where
    evalRPN' = traverse step (words str) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t   = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push
