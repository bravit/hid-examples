{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader

data Config = Config {
    verbose :: Bool
    {- other parameters -}
  }

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = pure Config { verbose = True {- ... -} }

main :: IO ()
main = do
  config <- getConfiguration
  let result = runReader work config
  print result

work :: ConfigM ()
work = do
  -- ...
  doSomething
  -- ...

doSomething :: ConfigM ()
doSomething = do
  -- ...
  doSomethingSpecial
  -- ...

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
  -- ...
  -- Config {verbose} <- ask
  vrb <- asks verbose
  when vrb beVerbose
  -- ...

beVerbose :: ConfigM ()
beVerbose = pure ()

silent :: Config -> Config
silent config = config {verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial
