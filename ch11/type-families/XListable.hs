{-# LANGUAGE TypeFamilies #-}

module XListable where

data family XList a
data instance XList () = XListUnit Integer
data instance XList Bool = XBits Integer Integer
data instance XList Char = XCons Char (XList Char) | XNil

class XListable a where
  xempty :: XList a
  xcons :: a -> XList a -> XList a
  xheadMay :: XList a -> Maybe a

instance XListable () where
  xempty = XListUnit 0

  xcons () (XListUnit n) = XListUnit (n + 1)

  xheadMay (XListUnit 0) = Nothing
  xheadMay _ = Just ()

instance XListable Bool where
  xempty = XBits 0 0

  xcons b (XBits bits n) = XBits (bits * 2 + if b then 1 else 0) (n + 1)

  xheadMay (XBits bits n)
    | n <= 0 = Nothing
    | otherwise = Just (bits `mod` 2 /= 0)

instance XListable Char where
  xempty = XNil
  xcons x xs = XCons x xs

  xheadMay (XCons c _) = Just c
  xheadMay XNil = Nothing

testXList :: (Eq a, XListable a) => a -> Bool
testXList a = xheadMay (xcons a xempty) == Just a

