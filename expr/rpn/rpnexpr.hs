import qualified EvalRPN as E
import qualified EvalRPNTrans as E1
import qualified EvalRPNTrans2 as E2

import qualified EvalRPNExcept as EE
import qualified EvalRPNExcept2 as EE2


import Control.Monad (mapM_, when)
import Data.Maybe

rpns = ["42",
        "12 13 +",
        "2 3 3 * + 5 *",
        "1 1 2 + 2 2 1 2 + * + * 1 3 2 * + + +",
        "13 2 12 2 1 2 13 2 + + + + + + +",
        "1 2 132 22 1 22 0 2 * * * * * * *",
        "10 1 2 + 2 2 1 2 * + * * * 1 3 2 + + +"]

showEvalRes :: String -> String
showEvalRes e = e ++ " = " ++ maybe "ERROR" show (E1.evalRPN e)

main = do
  print $ E.evalRPN "2 3 +"
  print $ E1.evalRPN "2 x +"
  print $ E2.evalRPN "x 3 +"
  let res1 = map E1.evalRPN rpns
  mapM_ (putStrLn . showEvalRes) rpns
  let    res2 = map E2.evalRPN rpns
  when (res1 == res2) $ putStrLn "E2 OK"

