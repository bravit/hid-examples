module Expr where

data Expr a = Lit a
            | Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)

myeval :: Num a => Expr a -> a
myeval (Lit e) = e
myeval (Add e1 e2) = myeval e1 + myeval e2
myeval (Mult e1 e2) = myeval e1 * myeval e2

instance Show a => Show (Expr a) where
  showsPrec _ (Lit a)  = shows a
  showsPrec p (Add e1 e2) = showParen (p > precAdd)
                            $ showsPrec precAdd e1
                              . showString "+" 
                              . showsPrec precAdd e2
    where precAdd = 5
  showsPrec p (Mult e1 e2) = showParen (p > precMult)
                             $ showsPrec precMult e1
                               . showString "*"
                               . showsPrec precMult e2
    where precMult = 6

expr = Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)

expr1 = Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)

expr2 = Add (Add (Lit 1) (Mult (Add (Lit 1) (Lit 2))
                               (Add (Lit 2)
                                    (Mult (Lit 2) (Add (Lit 1) (Lit 2))))))
            (Add (Lit 1) (Mult (Lit 3) (Lit 2)))
