{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module NFUtils where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import IPTypes

deriving instance Generic IP
deriving instance Generic IPRange
deriving instance Generic IPRangeDB

deriving instance NFData IP
deriving instance NFData IPRange
deriving instance NFData IPRangeDB

