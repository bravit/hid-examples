import Test.Tasty
import Test.Tasty.Hspec

import ParseIPSpec
import LookupIPSpec
import Props

main = do
  specs <- concat <$> mapM testSpecs
             [ parseIPSpecs
             , lookupIPSpecs  
             ]
  defaultMain (testGroup "All Tests" [
                  testGroup "Specs" specs
                , testGroup "Properties" props  
                ])
