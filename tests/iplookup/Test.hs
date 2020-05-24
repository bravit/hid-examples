import Test.Tasty
import Test.Tasty.Hspec

import ParseIPSpec
import LookupIPSpec
import Props
import GoldenTests

main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs
             [ parseIPSpecs
             , lookupIPSpecs
             ]
  goldens <- goldenTests
  defaultMain (testGroup "All Tests" [
                  testGroup "Specs" specs
                , testGroup "Properties" props
                , testGroup "Golden Tests" goldens
                ])
