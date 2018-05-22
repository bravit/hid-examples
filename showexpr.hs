import Text.Show
import Control.Monad
import Data.Foldable
import Language.Haskell.Interpreter

data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a)

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

myeval :: Num a => Expr a -> a
myeval (Lit e) = e
myeval (Add e1 e2) = myeval e1 + myeval e2
myeval (Mult e1 e2) = myeval e1 * myeval e2

testexpr e = do
  let e_str = show e
      e_val = myeval e
  putStr $ e_str ++ " = " ++ show e_val ++ " "
  r <- runInterpreter $ setImports ["Prelude"] >> eval e_str
  case r of
    Right r' -> if (read r' == e_val)
                   then putStrLn "ok"
                   else putStrLn "eval error"     
    _ -> putStrLn "interpreter error"

main = traverse_ testexpr exprs

exprs = [
  Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5),
  Add (Add (Lit 1) (Mult (Add (Lit 1) (Lit 2))
                           (Add (Lit 2) (Mult (Lit 2) (Add (Lit 1) (Lit 2))))))
      (Add (Lit 1) (Mult (Lit 3) (Lit 2))),
  Add (Add (Add (Lit 1) (Lit 2)) (Add (Lit 1) (Lit 2)))
      (Add (Add (Lit 1) (Lit 2)) (Add (Lit 1) (Lit 2))),
  Mult (Mult (Mult (Lit 1) (Lit 2)) (Mult (Lit 1) (Lit 2)))
       (Mult (Mult (Lit 1) (Lit 2)) (Mult (Lit 1) (Lit 2))),
  Add (Mult (Lit 1) (Mult (Add (Lit 1) (Lit 2))
                     (Mult (Lit 2) (Add (Lit 2) (Mult (Lit 1) (Lit 2))))))
      (Add (Lit 1) (Add (Lit 3) (Lit 2)))
         ]
