module Props where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog

import IPTypes
import GenIP
import ParseIP
import LookupIP

prop_buildIPs :: Property
prop_buildIPs = property $ do
    ipcs <- forAll genIPComponents
    let ip = buildIP ipcs
    buildIP' ipcs === ip
    buildIP'' ipcs === ip

prop_parseIP :: Property
prop_parseIP = property $ do
    ip <- forAll genIP
    parseIP (show ip) === Just ip

prop_parseIPRange :: Property
prop_parseIPRange = property $ do
    ipr <- forAll genIPRange
    parseIPRange (show ipr) === Just ipr

prop_parseIPRanges :: Property
prop_parseIPRanges = property $ do
    iprdb <- forAll genIPRangeDB
    iprdb' <- evalEither (parseIPRanges $ show iprdb)
    iprdb' === iprdb

prop_lookupIP_empty :: Property
prop_lookupIP_empty = property $ do
  ip <- forAll genIP
  assert (not $ lookupIP (IPRangeDB []) ip)

prop_lookupIP_bordersIncluded :: Property
prop_lookupIP_bordersIncluded = property $ do
  iprdb@(IPRangeDB iprdbs) <- forAll genIPRangeDB
  IPRange ip1 ip2 <- forAll $ Gen.element iprdbs
  assert (lookupIP iprdb ip1)
  assert (lookupIP iprdb ip2)

props = [
   testProperty "buildIP agrees with buildIP'" prop_buildIPs
 , testProperty "parseIP works as expected" prop_parseIP
 , testProperty "parseIPRange works as expected" prop_parseIPRange
 , testProperty "parseIPRanges works on generated lists" prop_parseIPRanges
 , testProperty "no ip in empty list" prop_lookupIP_empty
 , testProperty "lookupIP includes borders" prop_lookupIP_bordersIncluded
 ]
