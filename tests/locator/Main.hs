import System.Exit (exitFailure)

import Locator

main = do
  if test
    then putStrLn "OK"
    else exitFailure
