{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad.Reader

data Config = Config {
    flag1 :: Bool,
    flag2 :: Bool
    -- ...
  }

type ConfigM = Reader Config

main = do 
  let c = Config True False
  pure $ runReader work c
  print "OK"

work :: ConfigM ()
work = do
  -- ...
  f1
  -- ...

f1 :: ConfigM ()
f1 = do
  -- ...
  f11
  -- ...

f11 :: ConfigM ()
f11 = do
  -- ...
  c <- ask
  -- ...
  pure ()

nc = \c @ Config {flag1} -> c {flag1 = not flag1}

nc2 = \c -> c {flag1 = False}
