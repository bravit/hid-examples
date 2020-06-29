{-# LANGUAGE TemplateHaskell #-}

module Predicates (mkPredicates) where

import Language.Haskell.TH

mkPredicates :: Name -> Q [Dec]
mkPredicates name =
  reify name
  >>= fmap concat . mapM mkPredicate . extractConstructors

extractConstructors :: Info -> [Con]
extractConstructors (TyConI (DataD _ _ _ _ cons _)) = cons
extractConstructors _ = []

mkPredicate :: Con -> Q [Dec]
mkPredicate (NormalC name types) =
  [d|
     $predicate = \z -> case z of
                         $pat -> True
                         _ -> False
  |]
  where
    predicate = varP $ mkName $ "is" ++ nameBase name
    pat = conP name $ replicate (length types) wildP
mkPredicate _ = pure []
