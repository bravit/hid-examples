{-
 "/title/7548"
 ghci> get ["title", "7548"]
-}

get :: [String] -> IO String
get [] = pure "OK"
get [op, _] =
    case op of
       "title" -> pure "Haskell in Depth"
       "year" -> pure "2020"
       "rating" -> pure "Great"
       _ -> ioError (userError "Not implemented")
get _ = ioError (userError "Malformed request")

check :: IO ()
check = do
  b <- get []
  y <- get ["year", "7548"]
  putStrLn (if b == "OK" && y == "2020"
            then "OK"
            else "Wrong answer!")

main :: IO ()
main = check

