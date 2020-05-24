{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


import GHC.TypeLits
import Data.Proxy

-- Example: aligned pointers

newtype Pointer (align :: Nat) = Pointer Integer
  deriving Show

zeroPtr :: Pointer 4
zeroPtr = Pointer 0

otherPtr :: Pointer 8
otherPtr = Pointer 3


ptrValue :: forall align. KnownNat align => Pointer align -> Integer
ptrValue (Pointer p) = p * natVal (Proxy :: Proxy align)

inc :: Pointer align -> Pointer align
inc (Pointer p) = Pointer (p + 1)

setPtrValue :: forall align. KnownNat align => Integer -> Maybe (Pointer align)
setPtrValue p
  | reminder == 0 = Just (Pointer quotient)
  | otherwise = Nothing
  where
    (quotient, reminder) = divMod p (natVal (Proxy :: Proxy align))

-- Example: suffixed strings

data SuffixedString (suffix :: Symbol) = SS String


asString :: forall suffix. KnownSymbol suffix => SuffixedString suffix -> String
asString (SS str) = str ++ "@" ++ symbolVal (Proxy :: Proxy suffix)

main :: IO ()
main = undefined

