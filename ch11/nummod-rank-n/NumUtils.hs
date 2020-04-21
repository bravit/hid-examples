{-# LANGUAGE RankNTypes #-}

module NumUtils where

newtype NumModifier = NumModifier {
    run :: forall a. Num a => a -> a
  }
