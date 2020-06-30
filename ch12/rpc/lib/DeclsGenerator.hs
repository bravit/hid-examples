{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module DeclsGenerator (remote, genServer) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Network.Socket (PortNumber)

import RemoteParser

remote :: QuasiQuoter
remote =  QuasiQuoter {
    quoteExp = undefined,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = quoteFuncInfoDec
  }

quoteFuncInfoDec :: String -> Q [Dec]
quoteFuncInfoDec quote = parseRemoteInterface quote >>= genClientStubs

genClientStubs :: [FuncInfo] -> Q [Dec]
genClientStubs fis = concat <$> mapM (genClientStub "callRemote") fis

genClientStub :: String -> FuncInfo -> Q [Dec]
genClientStub callee FuncInfo {..} = do
    funcImpl <-  funD funName [clause [] (normalB stubBody) []]
    pure [typeSig, funcImpl]
  where
    funName = mkName name
    typeSig = SigD funName ty
    stubBody = [| $(curryArgs (arity ty)) $ $(dyn callee) name |]

    curryArgs 0 = [| \f -> f () |]
    curryArgs 1 = [| id |]
    curryArgs n = [| curry . $(curryArgs (n-1)) |]

genServer :: [Name] -> Q [Dec]
genServer names =
  [d|
    server :: String -> PortNumber -> IO ()
    server host port = serveRPC host port $(genRemoteTable names)
   |]

genRemoteTable :: [Name] -> Q Exp
genRemoteTable names =
  mapM reifyFunc names
  >>= listE . map (genServerStub "runSerialized")

genServerStub :: String -> FuncInfo -> ExpQ
genServerStub callee FuncInfo {..} =
    [| (name, $(dyn callee) $ $(prepareArgs (arity ty)) $(dyn name) ) |]
  where
    prepareArgs 0 = [| (const :: a -> () -> a) |]
    prepareArgs 1 = [| id |]
    prepareArgs n = [| uncurry . $(prepareArgs (n-1)) |]

reifyFunc :: Name -> Q FuncInfo
reifyFunc nm = do
  VarI _ t Nothing <- reify nm
  pure $ FuncInfo {
    name = nameBase nm,
    ty = t
  }

arity :: Type -> Int
arity (ForallT _ _ rest) = arity rest
arity (AppT (AppT ArrowT _) rest) = arity rest + 1
arity _ = 0
