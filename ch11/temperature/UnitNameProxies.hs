{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}

module UnitNameProxies where

import Data.Proxy
import TempPhantom

class UnitName u where
  unitName :: Proxy u -> String

instance UnitName C where
  unitName :: Proxy C -> String
  unitName _ = "C"

instance UnitName F where
  unitName :: Proxy F -> String
  unitName _ = "F"

instance UnitName unit => Show (Temp unit) where
  show (Temp t) = show t ++ "°" ++ unitName (Proxy :: Proxy unit)

instance UnitName Temp where
  unitName _ = "_unspecified unit_"

instance UnitName unit => UnitName (Temp unit) where
  unitName _ = unitName (Proxy :: Proxy unit)

unit :: forall u. UnitName u => Temp u -> String
unit _ = unitName (Proxy :: Proxy u)

someFunc :: proxy a -> String
someFunc _ = "OK"
