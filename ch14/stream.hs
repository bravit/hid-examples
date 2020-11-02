{-# LANGUAGE FlexibleContexts #-}

data Stream e m r = Element e (Stream e m r)
                  | Action (m (Stream e m r))
                  | Result r

empty :: Stream e m ()
empty = Result ()


stream :: Stream Int IO ()
stream = (Element 1
            (Action (putStrLn "some action" >>
                     pure (Element 2
                            (Action (putStrLn  "finish" >> pure empty))
                          )
                    )
            )
         )

printStream :: (Show e, Show r) => Stream e IO r -> IO ()
printStream (Result r) = putStrLn $ "Result: " <> show r
printStream (Element e str) = do
  putStrLn $ "Element: " <> show e
  printStream str
printStream (Action mstr) = do
  putStr "Run action: "
  str <- mstr
  printStream str

ssum :: (Num e, Monad m) => Stream e m r -> m (e, r)
ssum (Result r) = pure (0, r)
ssum (Action m) = m >>= ssum
ssum (Element e str) = (\(acc, r) -> (acc + e, r)) <$> ssum str

each :: [e] -> Stream e m ()
each [] = Result ()
each (x:xs) = Element x (each xs)

main :: IO ()
main = do
  printStream stream
  ssum stream >>= print
  ssum (each [1..10 :: Int]) >>= print
