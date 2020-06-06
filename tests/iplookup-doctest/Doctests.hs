import Test.DocTest

main :: IO ()
main = doctest ["-iip/lookup", "ip/lookup/ParseIP.hs"]
