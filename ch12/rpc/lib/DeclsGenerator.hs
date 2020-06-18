{-# LANGUAGE TemplateHaskell #-}
module DeclsGenerator (
        module Language.Haskell.TH,
        module Data.Serialize,
        genClientDecls,
        genServerDecls
    ) where

import Language.Haskell.TH
import qualified Language.Haskell.Exts as Exts
import Control.Monad
import Data.Serialize

--client needs Binary instances and function stubs
genClientDecls :: Q [Dec]
genClientDecls = do
    decls <- extractDecls
    let (datas, types) = filter2 isDataDecl isTypeSig decls
    liftM2 (++)
        (derivingBinaries datas)
        (genFuncStubs "callRemote" types)

--server needs Binary instances and registeredFunctions assoc array
genServerDecls :: Q [Dec]
genServerDecls = do
    decls <- extractDecls
    let (datas, types) = filter2 isDataDecl isTypeSig decls
    liftM2 (++)
        (derivingBinaries datas)
        (genRegisteredFunctions types)

----------------------------------------------------------------------------
-- Auxiliary functions

filter2 :: (a->Bool) -> (a->Bool) -> [a] -> ([a],[a])
filter2 p1 p2 xs = (filter p1 xs, filter p2 xs)

-- Exts.Decl type predicates

isTypeSig :: Exts.Decl l -> Bool
isTypeSig (Exts.TypeSig _ _ _) = True
isTypeSig _ = False

isDataDecl :: Exts.Decl l -> Bool
isDataDecl (Exts.DataDecl _ _ _ _ _ _) = True
isDataDecl _ = False

-- Extract all declarations from module under compilation
extractDecls :: Q [Exts.Decl Exts.SrcSpanInfo]
extractDecls = do
    -- get code
    loc <- location
    moduleCode <- runIO $ readFile $ loc_filename loc
    -- extract all declarations from moduleCode
    let decls = hsModuleDecls $ Exts.fromParseResult $
                    Exts.parseModuleWithMode
                        Exts.defaultParseMode {Exts.extensions=[Exts.EnableExtension Exts.TemplateHaskell]}
                        moduleCode
    return decls
  where
    hsModuleDecls (Exts.Module _ _  _ _ d) = d

funcNames :: [Exts.Decl Exts.SrcSpanInfo] -> [String]
funcNames = concat . map funNamesFromDecl
    where funNamesFromDecl (Exts.TypeSig _ ids _) = map (\(Exts.Ident _ n) -> n) ids

funcNamesWithTypes :: [Exts.Decl l] -> [(String, Exts.Type l)]
funcNamesWithTypes = concat . map funNamesFromDecl
    where funNamesFromDecl (Exts.TypeSig _ ids t) = map (\(Exts.Ident _ n) -> (n, t)) ids


dataNames :: [Exts.Decl l] -> [String]
dataNames = map (\(Exts.DataDecl _ _ _ (Exts.DHead _ (Exts.Ident _ name)) _ _) -> name)

-- Generates stubs for client
genFuncStubs :: String -> [Exts.Decl l] -> Q [Dec]
genFuncStubs calledFunction decls = mapM genStub (funcNamesWithTypes decls)
    where
        genStub (name, t)
             = funD (mkName name) [clause [] (normalB $ stubBody name (paramsCount t))  []]
        stubBody name 0 = [| $(dyn calledFunction) name () |]
        stubBody name 1 = [| $(dyn calledFunction) name |]
        stubBody name n = [| $(curries (n - 1)) $ $(dyn calledFunction) name |]
            where
                curries 1 = [| curry |]
                curries n = [| curry . $(curries (n-1)) |]

-- Generates list of registered functions for server
genRegisteredFunctions :: [Exts.Decl l] -> Q [Dec]
genRegisteredFunctions decls = [d| registeredFunctions = $funcList |]
    where 
        funcList = listE $ map name2Tup (funcNamesWithTypes decls)
        name2Tup (name, t)
            = [| (name, 
                $(dyn "run_serialized") $ $(funcKind t) ) |]
            where
                funcKind t | isAction t = [| $(calledFunc name (paramsCount t)) |]
                funcKind t | otherwise = [| return . $(calledFunc name (paramsCount t))|]
        calledFunc name 0 = [| (const :: a->()->a) $(dyn name) |]
        calledFunc name 1 = [| $(dyn name) |]
        calledFunc name n = [| $(uncurries (n-1)) $(dyn name) |]
            where
                uncurries 1 = [| uncurry |]
                uncurries n = [| uncurry . $(uncurries (n-1)) |]


-- Counts number of function's parameters
paramsCount :: Exts.Type l -> Int
paramsCount (Exts.TyFun _ t1 t2) = 1 + paramsCount t2
paramsCount _ = 0


--Determines if the type is an action (IO)
isAction :: Exts.Type l -> Bool
isAction (Exts.TyFun _ t1 t2) = isAction t2
isAction (Exts.TyApp _ t1 t2) = isAction t1
isAction (Exts.TyCon _ (Exts.Qual _ _ (Exts.Ident _ name))) = (name == "RemoteStIO") || (name == "RemoteIO")
isAction (Exts.TyCon _ (Exts.UnQual _ (Exts.Ident _ name))) = (name == "RemoteStIO") || (name == "RemoteIO")
isAction _ = False


-- Deriving Binary instances
derivingBinaries :: [Exts.Decl l] -> Q [Dec]
derivingBinaries decls = liftM msum $ mapM derivingBinary (dataNames decls)

data T1 = T1

derivingBinary :: String -> Q [Dec]
derivingBinary typename = do
    TyConI (DataD _ _ _ _ constructors _)  <-  reify (mkName typename)

    let genPE n = do
          ids <- replicateM n (newName "x")
          return (map varP ids, map varE ids)
    let putClause (n, NormalC name fields) = do
--        let constructorName = nameBase name
          (pats,vars) <- genPE (length fields)
          let action =
                (doE $ (noBindS (appE (varE (mkName "putWord8")) (litE (integerL n)))) :
                    map (\v -> noBindS (appE (varE (mkName "put")) v)) vars)
          clause [conP name pats] (normalB action) []

    putBody <- mapM putClause (zip [1..] constructors)
    let getMatch (n, NormalC name fields) = do
          (pats,vars) <- genPE (length fields)
          let returnOp = noBindS (appE (varE (mkName "return")) (foldl appE (conE name) vars ) )
          let gets = map (\p -> bindS p (varE (mkName "get")) ) pats
          let matchBody = normalB (doE $ gets ++ [returnOp])
          match (litP (integerL n)) matchBody []

    getBody <- do
            t <- newName "t"
            let getMatches = map getMatch (zip [1..] constructors)
            (normalB (doE $ [bindS (varP t) (varE (mkName "getWord8")),
                             noBindS (caseE (varE t) getMatches)]))

    d <- [d| instance Serialize T1 where
                 put _ = putWord8 1
                 get = return T1
         |]
    let [InstanceD l [] (AppT putt (ConT _T1)) [FunD putf _text, ValD getf _text2 _text3]] = d
    return [InstanceD l [] (AppT putt (ConT (mkName typename)  )) [FunD putf putBody, ValD getf getBody []]]

