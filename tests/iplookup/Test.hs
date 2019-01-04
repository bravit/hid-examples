import Test.Tasty
import Test.Tasty.Hspec

import ParseIPSpec
import LookupIPSpec
import Props
import GoldenTests

main = do
  specs <- concat <$> mapM testSpecs
             [ parseIPSpecs
             , lookupIPSpecs  
             ]
  gts <- golden_tests           
  defaultMain (testGroup "All Tests" [
                  testGroup "Specs" specs
                , testGroup "Properties" props  
                , testGroup "Golden Tests" [gts]  
                ])
