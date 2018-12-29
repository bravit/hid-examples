import Test.Tasty
import Test.Tasty.Hspec

import ParseIPSpec
import LookupIPSpec


main = do
  specs <- concat <$> mapM testSpecs
             [ parseIPSpecs
             , lookupIPSpecs  
             ]
  defaultMain (testGroup "Specs" specs)
