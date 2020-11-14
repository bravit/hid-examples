import Streaming
import System.Environment
import Streaming.ByteString as BS
import Control.Monad.Trans.Resource
import Data.Function ((&))
import System.FilePath

copyFile' :: FilePath -> FilePath -> IO Int
copyFile' fIn fOut = do
  (len :> ()) <- runResourceT
               $ BS.writeFile fOut
               $ BS.length
               $ BS.copy
               $ BS.readFile fIn
  pure len


copyFile :: FilePath -> FilePath -> IO Int
copyFile fIn fOut = runResourceT $ do
  (len :> ()) <- BS.readFile fIn
               & BS.copy
               & BS.length
               & BS.writeFile fOut
  pure len

main :: IO ()
main = do
  [fp] <- getArgs
  let copyName = replaceBaseName fp (takeBaseName fp <> ".copy")
  len <- copyFile fp copyName

  putStrLn $ show len <> " bytes copied"

{-
Testing:

dd if=/dev/urandom of=temp_1GB_file bs=1 count=0 seek=1g
time cabal -v0 run copy -- temp_1GB_file
-}
