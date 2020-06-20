{-# LANGUAGE RecordWildCards #-}
module IfaceExtractor (FuncInfo (..), extractFunctions) where

import Language.Haskell.Exts
import Language.Haskell.Meta.Syntax.Translate (toDec)
import Language.Haskell.TH (Dec)

data FuncInfo = FuncInfo {
    name :: String
  , numParams :: Int
  , typeSig :: Dec
  }

extractFunctions :: FilePath -> IO [FuncInfo]
extractFunctions fpath = concat . map typeSigs <$> extractDecls
  where
    extractDecls = readFile fpath
                   >>= pure . hsModuleDecls . parse
    withTH = defaultParseMode {extensions=[EnableExtension TemplateHaskell]}
    parse = fromParseResult . parseModuleWithMode withTH
    hsModuleDecls (Module _ _  _ _ d) = d

typeSigs :: Decl SrcSpanInfo -> [FuncInfo]
typeSigs ts@(TypeSig _ ids ty) =
      [FuncInfo {..} | Ident _ name <- ids,
                       let numParams = paramsCount ty,
                       let typeSig = toDec ts]
typeSigs _ = []

-- Counts number of function's parameters
paramsCount :: Type l -> Int
paramsCount (TyFun _ t1 t2) = 1 + paramsCount t2
paramsCount _ = 0
