{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module UnitNameProxies where

import Data.Proxy
import TempPhantom

class UnitName a where
  unitName :: Proxy a -> String

instance UnitName C where
  unitName _ = "C"

instance UnitName F where
  unitName _ = "F"

instance UnitName u => Show (Temp u) where
  show (Temp t) = show t ++ "°" ++ unitName (Proxy :: Proxy u)

instance UnitName Temp where
  unitName _ = "_unspecified unit_"

instance UnitName u => UnitName (Temp u) where
  unitName _ = unitName (Proxy :: Proxy u)

unitName' :: forall u. UnitName u => Temp u -> String
unitName' _ = unitName (Proxy :: Proxy u)
