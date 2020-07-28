module ShuntingYard (convertToExpr) where

import Data.Char (isDigit, isSpace)
import Data.List (groupBy)
import Data.Foldable (traverse_)
import Control.Monad.State

import Expr

-- Implementation of the Shunting-yard algorithm
-- https://en.wikipedia.org/wiki/Shunting-yard_algorithm

type Token = String
type Stack = [Token]
type Output = [Expr Integer]
type SYState = (Stack, Output)

isEmpty :: State SYState Bool
isEmpty = null <$> gets fst

notEmpty :: State SYState Bool
notEmpty = not <$> isEmpty

top :: State SYState Token
top = gets (head . fst) -- let it crash on empty stack

pop :: State SYState Token
pop = do
  (s, es) <- get
  put (tail s, es) -- let it crash on empty stack
  pure (head s)

pop_ :: State SYState ()  -- let it crash on empty stack
pop_ = modify (\(s, es) -> (tail s, es))

push :: Token -> State SYState ()
push t = modify (\(s, es) -> (t : s, es))

whileNotEmptyAnd :: (Token -> Bool) -> State SYState () -> State SYState ()
whileNotEmptyAnd predicate m = go
  where
    go = do
      b1 <- notEmpty
      when b1 $ do
        b2 <- predicate <$> top
        when b2 (m >> go)

output :: Token -> State SYState ()
output t = modify (builder t <$>)
  where
    builder "+" (e1 : e2 : es) = Add e1 e2 : es
    builder "*" (e1 : e2 : es) = Mult e1 e2 : es
    builder n es = Lit (read n) : es -- let it crash on not a number

isOp :: String -> Bool
isOp "+" = True
isOp "*" = True
isOp _ = False

precedence :: String -> Int
precedence "+" = 1
precedence "*" = 2
precedence _ = 0

precGTE :: String -> String -> Bool
t1 `precGTE` t2 = precedence t1 >= precedence t2

convertToExpr :: String -> Expr Integer
convertToExpr str = head $ snd $ execState shuntingYard ([], [])
  where
    tokens = reverse $ tokenize str

    shuntingYard = traverse_ processToken tokens >> transferRest

    processToken ")" = push ")"
    processToken "(" = transferWhile (/= ")") >> pop_
    processToken t
      | isOp t = transferWhile (`precGTE` t) >> push t
      | otherwise = output t -- number

    transfer = pop >>= output
    transferWhile predicate = whileNotEmptyAnd predicate transfer
    transferRest = transferWhile (const True)

    tokenize = groupBy (\a b -> isDigit a && isDigit b)
               . filter (not . isSpace)
