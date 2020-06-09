{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SuffixedStrings (SuffixedString, suffixed, asString) where

import GHC.TypeLits
import Data.Proxy

-- Example: suffixed strings

data SuffixedString (suffix :: Symbol) = SS String

suffixed :: String -> SuffixedString suffix
suffixed s = SS s

asString :: forall suffix. KnownSymbol suffix =>
            SuffixedString suffix -> String
asString (SS str) = str ++ "@" ++ symbolVal (Proxy :: Proxy suffix)
