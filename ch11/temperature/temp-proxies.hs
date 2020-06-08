import TempPhantom
import UnitNameProxies

printTemp :: UnitName u => Temp u -> IO ()
printTemp t = do
  putStrLn $ "Temperature: " ++ show t
  putStrLn $ "Units: " ++ unitName' t

main :: IO ()
main = do
  printTemp paperBurning
  printTemp absoluteZero
