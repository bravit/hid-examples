import System.Exit (exitFailure)
import Data.Foldable (traverse_)

import Language.Haskell.Interpreter

import Expr

testexpr :: (Eq a, Read a, Show a, Num a) => Expr a -> IO ()
testexpr e = do
  let e_str = show e
      e_val = myeval e
  putStr $ e_str ++ " = " ++ show e_val ++ " "
  r <- runInterpreter $ setImports ["Prelude"] >> eval e_str
  case r of
    Right r' -> if read r' == e_val
                   then putStrLn "ok"
                   else failedTest "eval error"
    _ -> failedTest "interpreter error"

failedTest msg = do
  putStrLn msg
  exitFailure

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
