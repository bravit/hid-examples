{-# LANGUAGE RecordWildCards #-}

module RemoteParser where

import Language.Haskell.Exts hiding (Type, name)
import Language.Haskell.Meta.Syntax.Translate (toType)
import Language.Haskell.TH (Type, Q)
import Data.Char

data FuncInfo = FuncInfo {
    name :: String
  , ty :: Type
  }

parseRemoteInterface :: String -> Q [FuncInfo]
parseRemoteInterface quote = concat <$> mapM (funcInfo . parseDecl) funcs
  where
    funcs = filter (not . null) $ map (dropWhile isSpace) $ lines quote

funcInfo :: ParseResult (Decl SrcSpanInfo) -> Q [FuncInfo]
funcInfo (ParseOk (TypeSig _ ids t)) =
   pure $ [FuncInfo {..} | Ident _ name <- ids,
                       let ty = toType t]
funcInfo err =
  fail $ "Error when parsing remote interface (type signature expected)\n"
         <> show err
