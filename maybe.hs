import Safe

type Name = String
type Phone = String
type Location = String
type PhoneNumbers = [(Name, Phone)]
type Locations = [(Phone, Location)]

locByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locByName pnumbers locs name = lookup name pnumbers >>= flip lookup locs

locByName' :: PhoneNumbers -> Locations -> Name -> Maybe Location
locByName' pnumbers locs name = case lookup name pnumbers of
                                   Just number -> lookup number locs
                                   Nothing -> Nothing

doubleStrNumber :: (Num a, Read a) => String -> Maybe a
doubleStrNumber s = (*2) <$> readMay s


plusStrNumbers :: (Num a, Read a) => String -> String -> Maybe a
plusStrNumbers s1 s2 = (+) <$> readMay s1 <*> readMay s2

main = do
  print $ doubleStrNumber "21"
  print $ plusStrNumbers "10" "xx"
  
