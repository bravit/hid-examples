import Data.Char
import Data.List
import Data.Foldable
import Control.Monad.State

-- Implementation of the Shunting-yard algorithm

data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a)

type Token = String
type Stack = [Token]
type Output = [Expr Integer]
type MyState = (Stack, Output)

push :: Token -> State MyState ()
push t = modify (\(s, es) -> (t : s, es))

pop :: State MyState Token
pop = do
  (t : s, es) <- get -- let it crash on empty stack
  put (s, es)
  pure t

pop_ :: State MyState ()  -- let it crash on empty stack
pop_ = modify (\(s, es) -> (tail s, es))

top :: State MyState Token
top = gets (head . fst) -- let it crash on empty stack

isEmpty :: State MyState Bool
isEmpty = null <$> gets fst

notEmpty :: State MyState Bool
notEmpty = not <$> isEmpty

output :: Token -> State MyState ()
output t = modify (builder t <$>)
  where 
    builder "+" (e1 : e2 : es) = Add e1 e2 : es
    builder "*" (e1 : e2 : es) = Mult e1 e2 : es
    builder n es = Lit (read n) : es -- let it crash on not a number

whileNotEmptyAnd :: (Token -> Bool) -> State MyState () -> State MyState ()
whileNotEmptyAnd pred m = go
  where
    go = do
      b1 <- notEmpty
      when b1 $ do
        b2 <- pred <$> top
        when b2 (m >> go)

isOp "+" = True
isOp "*" = True
isOp _ = False

precedence "+" = 1
precedence "*" = 2
precedence _ = 0

t1 `precGTE` t2 = precedence t1 >= precedence t2 

convertToExpr :: String -> Expr Integer
convertToExpr str = head $ snd $ execState convert ([], [])
  where
    convert = traverse_ processToken (reverse $ tokenize str) >> transferRest

    processToken ")" = push ")"
    processToken "(" = transferWhile (/= ")") >> pop_
    processToken t
      | isOp t = transferWhile (`precGTE` t) >> push t
      | otherwise = output t -- number

    transfer = pop >>= output
    transferWhile pred = whileNotEmptyAnd pred transfer
    transferRest = transferWhile (const True)
    
    tokenize = groupBy (\a b -> isDigit a && isDigit b)
               . filter (not . isSpace)

{-

https://en.wikipedia.org/wiki/Shunting-yard_algorithm

* stack
* output
* reading tokens in reversed order

while there are tokens to be read:
  read a token.
  if the token is a right bracket (i.e. ")"), then:
    push it onto the stack.
  if the token is a left bracket (i.e. "("), then:
    while the operator at the top of the stack is not a right bracket:
      pop the operator from the stack onto the output.
    pop the right bracket from the stack.
    /* if the stack runs out without finding a right bracket,
       then there are mismatched parentheses. */
  if the token is an operator, then:
    while ((there is an operator at the top of the stack
                        with equal or greater precedence)
           and (the operator at the top of the stack
                is not a left bracket):
       pop operators from the stack onto the output.
    push it onto the stack.
  if the token is a number, then:
    push it to the output.

if there are no more tokens to read:
  while there are still operator tokens on the stack:
    /* if the operator token on the top of the stack is a bracket,
       then there are mismatched parentheses. */
    pop the operator from the operator stack onto the output.
exit.

-}

-- Printing `Expr a` values

instance Show a => Show (Expr a) where
  showsPrec _ (Lit a)  = shows a
  showsPrec p (Add e1 e2) = showParen (p > precAdd)
                            $ showsPrec precAdd e1
                              . showString "+" 
                              . showsPrec precAdd e2
    where precAdd = 5
  showsPrec p (Mult e1 e2) = showParen (p > precMult)
                             $ showsPrec (precMult) e1
                               . showString "*"
                               . showsPrec (precMult) e2
    where precMult = 6

-- Evaluating expressions

myeval :: Num a => Expr a -> a
myeval (Lit e) = e
myeval (Add e1 e2) = myeval e1 + myeval e2
myeval (Mult e1 e2) = myeval e1 * myeval e2

-- Testing

strs = ["42", "12 + 13", "(2+3*3)*5", "1+(1+2)*(2+2*(1+2))+1+3*2",
        "13+2+12+2+1+2+13+2", "1*2*132*22*1*22*0*2", "10*(1+2)*2*(2+1*2)+1+3+2"]

view = traverse_ printExpr strs
  where
    printExpr s = do
      let e = convertToExpr s
      putStrLn $ show e ++ "=" ++ show (myeval e)

exprs = map convertToExpr strs
exprs' = map (convertToExpr . show) exprs

check = and $ zipWith (\e1 e2 -> myeval e1 == myeval e2) exprs exprs'

-- Converting expressions to prefix and postfix forms

data ExprForm = Prefix | Postfix

exprTo _ (Lit a) = show a
exprTo form (Add e1 e2) = binOp "+" form e1 e2
exprTo form (Mult e1 e2) = binOp "*" form e1 e2

binOp op form e1 e2 = concat $ intersperse " " (args form) 
   where
     e1' = exprTo form e1
     e2' = exprTo form e2
     args Prefix = [op, e1', e2']
     args Postfix = [e1', e2', op]

main = do
  view
  putStr "Checked: "
  print check
  putStrLn "\nPrefix forms: "
  mapM_ (putStrLn.exprTo Prefix) exprs
  putStrLn "\nPostfix forms: "
  mapM_ (putStrLn.exprTo Postfix) exprs
