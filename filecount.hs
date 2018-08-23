import Data.Foldable (traverse_)
import System.Environment (getArgs)
import System.Directory.Extra
import Control.Monad.Extra (whenM, ifM, zipWithM)
import Data.IORef (newIORef, modifyIORef', readIORef)

fileCount :: FilePath -> IO Int
fileCount fp = do
   cnt <- newIORef 0
   whenM (doesDirectoryExist fp) $ go cnt fp 
   readIORef cnt
 where
   go cnt fp = listContents fp >>= traverse_ (processEntry cnt)
   processEntry cnt fp = ifM (doesDirectoryExist fp) (go cnt fp) (inc cnt)
   inc cnt = modifyIORef' cnt (+ 1)

fileCount' :: FilePath -> IO Int
fileCount' fp = length <$> listFilesRecursive fp

main = do
   args <- getArgs
   xs <- traverse fileCount args
   zipWithM printEntry args xs
 where
   printEntry fp n = putStrLn (show n ++ "\t" ++ fp)
