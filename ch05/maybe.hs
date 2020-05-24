import Text.Read (readMaybe)

type Name = String
type Phone = String
type Location = String
type PhoneNumbers = [(Name, Phone)]
type Locations = [(Phone, Location)]

doubleStrNumber1 :: (Num a, Read a) => String -> Maybe a
doubleStrNumber1 str =
  case readMaybe str of
    Just x -> Just (2*x)
    Nothing -> Nothing

doubleStrNumber2 :: (Num a, Read a) => String -> Maybe a
doubleStrNumber2 s = (2*) `fmap` readMaybe s


plusStrNumbers :: (Num a, Read a) => String -> String -> Maybe a
plusStrNumbers s1 s2 = (+) <$> readMaybe s1 <*> readMaybe s2

locateByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName pnumbers locs name =
  lookup name pnumbers >>= flip lookup locs

locateByName' :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName' pnumbers locs name =
  case lookup name pnumbers of
    Just number -> lookup number locs
    Nothing -> Nothing

main :: IO ()
main = do
  print (doubleStrNumber1 "21" :: Maybe Int)
  print (doubleStrNumber2 "21" :: Maybe Int)
  print (plusStrNumbers "10" "xx" :: Maybe Int)

