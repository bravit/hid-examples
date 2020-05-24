{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

class Describe a where
  describe :: String

instance Describe Bool where
  describe = "My favorite type"

data MyType

instance Describe MyType where
  describe = "My own type"

instance Describe Maybe where
  describe = "Maybe type constructor"

answer1, answer2, answer3 :: String

answer1 = describe @Bool
answer2 = describe @MyType
answer3 = describe @Maybe

main :: IO ()
main = do
  putStrLn answer1
  putStrLn answer2
  putStrLn answer3

