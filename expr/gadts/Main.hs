{-# LANGUAGE GADTs #-}

data Expr' a = Lit' a | Add' (Expr' a) (Expr a) | Mult' (Expr' a) (Expr' a)

data Expr'' a where
  Lit'' :: a -> Expr'' a
  Add'' :: Expr'' a -> Expr'' a -> Expr'' a
  Mult'' :: Expr'' a -> Expr'' a -> Expr'' a

data Expr a where
  NumLit :: Num a => a -> Expr a -- CHANGED
  BoolLit :: Bool -> Expr Bool   -- NEW
  Add :: Num a => Expr a -> Expr a -> Expr a
  Mult :: Num a => Expr a -> Expr a -> Expr a
  IsZero :: (Num a, Eq a) => Expr a -> Expr Bool  -- NEW
  If :: Expr Bool -> Expr a -> Expr a -> Expr a  -- NEW

myeval :: Expr a -> a
myeval (NumLit e) = e
myeval (BoolLit b) = b
myeval (Add e1 e2) = myeval e1 + myeval e2
myeval (Mult e1 e2) = myeval e1 * myeval e2
myeval (IsZero e) = myeval e == 0
myeval (If be e1 e2) = myeval (if myeval be then e1 else e2)

expr1 = Add (NumLit 5) (NumLit (-5))
expr2 = If (IsZero expr1) (NumLit 0.5) (NumLit 1)

data SomeExpr where
  Some :: Expr a -> SomeExpr

main :: IO ()
main = print $ myeval $ expr2
