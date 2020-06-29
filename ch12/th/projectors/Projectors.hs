{-# LANGUAGE TemplateHaskell #-}
module Projectors where

import Language.Haskell.TH

proj :: Int -> Int -> Q Exp
{-
proj n k
 | n > 1 && 0 <= k && k < n = do
    x <- newName "x"
    let mkPat j
          | j == k = VarP x
          | otherwise = WildP
    pure $ LamE [TupP $ map mkPat [0..n-1]] (VarE x)
 | n <=1 = fail "Wrong number of tuple elements (must be > 1)"
 | otherwise = fail $ "Incorrect projection: "
               <> show k <> " of " <> show n <> " elements"
-}
{-
proj n k = do
   x <- newName "x"
   let mkPat j
         | j == k = VarP x
         | otherwise = WildP
   pure $ LamE [TupP $ map mkPat [0..n-1]] (VarE x)
-}


proj n k
  | n > 1 && 0 <= k && k < n = do
     x <- newName "x"
     [| \ $(mkArg x) -> $(varE x) |]
  | n <=1 = fail "Wrong number of tuple elements (must be > 1)"
  | otherwise = fail $ "Incorrect projection: "
               <> show k <> " of " <> show n <> " elements"
  where
    mkPat x j
      | j == k = varP x
      | otherwise = wildP
    mkArg x = tupP $ map (mkPat x) [0..n-1]

mkProjName :: Int -> Int -> Name
mkProjName n k = mkName $ "proj_" <> show n <> "_" <> show k

mkProjDec :: Int -> Int -> Q [Dec]
mkProjDec n k = [d| $nm = $(proj n k) |]
  where
    nm = varP $ mkProjName n k

mkProjType :: Int -> Int -> Q Dec
mkProjType n k = sigD nm funTy
  where
    nm = mkProjName n k

    funTy = do
      resTy <- newName "res"
      tys <- mapM (getTy resTy) [0..n-1]
      forallT (map plainTV tys)
              (pure [])
              [t| $(mkTuple tys) -> $(varT resTy) |]

    getTy resTy j
      | k == j = pure resTy
      | otherwise = newName "ty"

    mkTuple tys = pure $ foldl addApp (TupleT n) tys
    addApp acc_ty ty = AppT acc_ty (VarT ty)

mkProjectors :: [Int] -> Q [Dec]
mkProjectors = fmap concat . mapM projectors
  where
    projectors n = concat <$> mapM (mkProj n) [0..n-1]
    mkProj n k = (:) <$> mkProjType n k <*> mkProjDec n k

{-

ForallT [PlainTV a_0,PlainTV b_1] []
  (AppT
     (AppT
         ArrowT
         (AppT
             (AppT (TupleT 2) (VarT a_0))
             (VarT b_1)
         )
     )
     (VarT a_0)
  )

-}
