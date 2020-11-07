import Control.Concurrent (getNumCapabilities)

main :: IO ()
main = do
  n <- getNumCapabilities
  putStrLn $ "This program runs over " ++ show n ++ " capabilities"
