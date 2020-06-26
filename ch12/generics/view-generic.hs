{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import GHC.Generics

data Status = Ok | Err
  deriving (Show, Eq, Generic)

data Request = Request String Int
  deriving Generic

okVal = from Ok

errVal :: D1
       ('MetaData "Status" "Main" "main" 'False)
       (C1 ('MetaCons "Ok" 'PrefixI 'False) U1
        :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1)
       x
errVal = M1 {unM1 = R1 (M1 {unM1 = U1})}

main :: IO ()
main = do
  print $ from Ok
  print $ from Err
  print $ Err == to errVal
