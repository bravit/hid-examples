{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}

import Data.Proxy

-- data Proxy t = Proxy

class DescribeType a where
  describe :: Proxy a -> String

instance DescribeType Bool where
  describe :: Proxy Bool -> String
  describe _ = "My favorite type"

data MyType

instance DescribeType MyType where
  describe :: Proxy MyType -> String
  describe _ = "My own type"


-- We need the PolyKinds extension to define this instance  
instance DescribeType Maybe where
  describe _ = "Maybe type constructor"

-- class JSONSchema a where
--   schema :: Proxy a -> Schema  

someFunc :: proxy a -> String
someFunc _ = "OK"

main = undefined
