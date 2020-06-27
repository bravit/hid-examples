{-# LANGUAGE TemplateHaskell #-}
module Pr where
import Language.Haskell.TH


{-
TODO:
* generate type declaration
* remove -Wno-missing-signatures
-}

proj :: Int -> Int -> Q Exp
{-
proj n k
 | n > 1 && 0 <= k && k < n = do
    x <- newName "x"
    let mkPat j
          | j == k = VarP x
          | otherwise = WildP
    pure $ LamE [TupP $ map mkPat [0..n-1]] (VarE x)
 | otherwise = fail "impossible projection"
-}

proj n k = do
   x <- newName "x"
   let mkPat j
         | j == k = VarP x
         | otherwise = WildP
   pure $ LamE [TupP $ map mkPat [0..n-1]] (VarE x)


{-
proj n k
  | n > 1 && 0 <= k && k < n = do
     x <- newName "x"
     let mkPat j
          | j == k = varP x
          | otherwise = wildP
     [| \ $(tupP $ map mkPat [0..n-1]) -> $(varE x) |]
  | otherwise = fail "impossible projection"

-}

projections :: Int -> Q [Dec]
projections n = fmap concat $ mapM mkDecl [0..n-1] where
  mkDecl k = do
    let nm = mkName $ "proj_" ++ show n ++ "_" ++ show k
    [d| $(varP nm) = $(proj n k) |]

mkProjections :: [Int] -> Q [Dec]
mkProjections = fmap concat . mapM projections
