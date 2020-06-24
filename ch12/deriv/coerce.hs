{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}

import Data.Coerce
import Unsafe.Coerce

newtype Age = Age Int
  deriving (Show)

toAges :: [Int] -> [Age]
toAges = map Age

toAges' :: [Int] -> [Age]
toAges' = coerce

data Student ageType = Student String ageType
--type role Student nominal

check :: Student Int -> Student Age
check = coerce

data Student1 ageType = Student1 String (Maybe ageType)

check1 :: Student1 Int -> Student1 Age
check1 = coerce

data Student2 m ageType = Student2 String (m ageType)

-- Can't coerce:
{-
check2 :: Student2 Maybe Int -> Student2 Maybe Age
check2 = coerce
-}


type family Id t
type instance Id t = t

data Student3 ageType = Student3 String (Id ageType)

-- Can't coerce:
-- {-
-- check3 :: Student3 Int -> Student3 Age
-- check3 = coerce
-- -}

check3' :: Student3 Int -> Student3 Age
check3' = unsafeCoerce

main :: IO ()
main = do
  print $ toAges [20, 30, 40, 50]
  print $ toAges' [20, 30, 40, 50]
