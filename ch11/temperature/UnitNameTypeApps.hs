{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitNameTypeApps where

import TempPhantom

class UnitName a where
  unitName :: String

instance UnitName C where
  unitName = "C"

instance UnitName F where
  unitName = "F"

instance UnitName Temp where
  unitName = "_unspecified unit_"

instance UnitName u => UnitName (Temp u) where
  unitName = unitName @u

instance UnitName u => Show (Temp u) where
  show (Temp t) = show t ++ "°" ++ unitName @u

unitName' :: forall u. UnitName u => Temp u -> String
unitName' _ = unitName @u
