{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Logger

popAndLog :: LoggingT (StateT [Int] IO) ()
popAndLog = do
  _:xs <- lift get
  lift (put xs)
  $logDebug ("***" <> (pack $ show xs) <> "***")

logStateEx :: LoggingT (StateT [Int] IO) Int
logStateEx =  do
  popAndLog
  popAndLog
  pure 5

main :: IO ()
main = runStateT (runStdoutLoggingT logStateEx) [1,2,3] >>= print
