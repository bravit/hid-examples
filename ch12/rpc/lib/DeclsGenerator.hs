{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module DeclsGenerator (
        genClientDeclsFrom,
        genServerDecls
    ) where

import Language.Haskell.TH
import IfaceExtractor

--client needs function stubs
genClientDeclsFrom :: FilePath -> Q[Dec]
genClientDeclsFrom fpath = loadIfaceFrom fpath >>= genClientStubs

--server needs registeredFunctions assoc array
genServerDecls :: Q [Dec]
genServerDecls = loadIface >>= genRegisteredFunctions "runSerialized"

loadIfaceFrom :: FilePath -> Q [FuncInfo]
loadIfaceFrom fpath = runIO $ extractFunctions fpath

loadIface :: Q [FuncInfo]
loadIface = location >>= loadIfaceFrom . loc_filename

genClientStubs :: [FuncInfo] -> Q [Dec]
genClientStubs fis = concat <$> mapM (genClientStub "callRemote") fis

genClientStub :: String -> FuncInfo -> Q [Dec]
genClientStub callee FuncInfo {..} = do
    funcImpl <-  funD (mkName name) [clause [] (normalB stubBody) []]
    pure [typeSig, funcImpl]
  where
    stubBody = [| $(curryArgs numParams) $ $(dyn callee) name |]

    curryArgs 0 = [| \f -> f () |]
    curryArgs 1 = [| id |]
    curryArgs n = [| curry . $(curryArgs (n-1)) |]

genRegisteredFunctions :: String -> [FuncInfo] -> Q [Dec]
genRegisteredFunctions callee funcs =
    [d| registeredFunctions = $(listE $ map (genServerStub callee) funcs) |]

genServerStub :: String -> FuncInfo -> ExpQ
genServerStub callee FuncInfo {..} =
    [| (name, $(dyn callee) $ $(prepareArgs numParams) $(dyn name) ) |]
  where
    prepareArgs 0 = [| (const :: a -> () -> a) |]
    prepareArgs 1 = [| id |]
    prepareArgs n = [| uncurry . $(prepareArgs (n-1)) |]
