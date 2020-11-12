{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ == 810

main :: IO ()
main = putStrLn "not implemented"

#else

import System.Exit (exitFailure)
import Data.Foldable (traverse_)

import Language.Haskell.Interpreter

import TextShow
import Expr

testexpr :: TextShow a => Expr a -> a -> IO ()
testexpr e r = do
  let e_str = toString $ showb e
      r_str = toString $ showb r
  putStr $ e_str ++ " = " ++ r_str ++ " "
  res <- expectInterpResult e_str r_str
  case res of
    Right () -> putStrLn "ok"
    Left err -> failedTest err

expectInterpResult :: String -> String -> IO (Either String ())
expectInterpResult expr expected = do
  r <- runInterpreter $ setImports ["Prelude"] >> eval expr
  case r of
    Right res -> if res == expected
                   then pure $ Right ()
                   else pure $ Left "eval error"
    Left _ -> pure $ Left "interpreter error"

failedTest :: String -> IO b
failedTest msg = do
  putStrLn msg
  exitFailure

main :: IO ()
main = traverse_ (\e -> testexpr e (myeval e)) exprs

exprs :: [Expr Int]
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

#endif
