{-# LANGUAGE GADTs #-}

data DynValue a where
  S :: String -> DynValue String
  C :: Char -> DynValue Char
  B :: Bool -> DynValue Bool

getValue :: DynValue a -> a
getValue (B b) = b
getValue (C c) = c
getValue (S s) = s

printValue :: DynValue a -> IO ()
printValue (B b) = print b
printValue (C c) = print c
printValue (S s) = print s

data WrappedDynValue where
  Wrap :: DynValue a -> WrappedDynValue

fromString :: String -> WrappedDynValue
fromString str
  | str `elem` ["y", "yes", "true"] = Wrap (B True)
  | str `elem` ["n", "no", "false"] = Wrap (B False)
  | length str == 1 = Wrap (C $ head str)
  | otherwise = Wrap (S str)

printWDValue :: WrappedDynValue -> IO ()
printWDValue (Wrap dv) = printValue dv

main :: IO ()
main = mapM_ (printWDValue . fromString) ["y", "no", "xxx", "c"]
