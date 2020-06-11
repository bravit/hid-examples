{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module SimplifyWiden where


type family Simplify t

type instance Simplify Integer = Integer
type instance Simplify Int = Integer
type instance Simplify Double = Integer
type instance Simplify String = String
type instance Simplify Char = String
type instance Simplify Bool = String

class Simplifier t where
  simplify :: t -> Simplify t

instance Simplifier Integer where
  simplify = id

instance Simplifier Int where
  simplify = fromIntegral

instance Simplifier Double where
  simplify = round

instance Simplifier String where
  simplify = id

instance Simplifier Bool where
  simplify = show

instance Simplifier Char where
  simplify = (:"")

type family Widen a where
  Widen Bool = Int
  Widen Int = Integer
  Widen Char = String
  Widen t = String

class Widener a where
  widen :: a -> Widen a

instance Widener Bool where
  widen False = 0
  widen True = 1

instance Widener Int where
  widen a = fromIntegral a

instance Widener Char where
  widen c = [c]

instance Widener Double where
  widen = show
