{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Pointers
import SuffixedStrings

main :: IO ()
main = do
    print $ ptrValue $ somePtr -- must be 8
    print $ ptrValue <$> mbptr1
    print $ ptrValue <$> mbptr2
    putStrLn $ asString id1
    putStrLn $ asString id2
  where
    somePtr :: Pointer 4
    somePtr = inc $ inc zeroPtr

    mbptr1 :: Maybe (Pointer 8)
    mbptr1 = maybePtr 24

    mbptr2 :: Maybe (Pointer 8)
    mbptr2 = maybePtr 42

    id1 :: SuffixedString "teachers"
    id1 = suffixed "bravit"

    id2 :: SuffixedString "devs"
    id2 = suffixed "bravit"
