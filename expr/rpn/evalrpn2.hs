import System.Environment

import EvalRPNTrans

import Control.Monad (mapM_)

evalPrintExpr :: String -> IO ()
evalPrintExpr str = do
  let r = evalRPN str
  putStrLn $ str ++ " = " ++ show r

main = getArgs >>= mapM_ evalPrintExpr
