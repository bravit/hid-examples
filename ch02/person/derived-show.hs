{-# LANGUAGE StandaloneDeriving #-}

import Person

deriving instance Show Person
deriving instance Read Person
deriving instance Eq Person

main :: IO ()
main = do
  print homer
  print spj

