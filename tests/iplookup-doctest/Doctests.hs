import Test.DocTest

main :: IO ()
main = doctest ["-iiplookup", "iplookup/ParseIP.hs"]
