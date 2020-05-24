import Data.Foldable (traverse_)
import Control.Monad (when)
import Control.Monad.ST
import Data.STRef

countZeros :: [Int] -> Int
countZeros = length . filter (== 0)

countZerosST :: [Int] -> Int
countZerosST xs = runST $ do
   c <- newSTRef 0
   traverse_ (\x -> when (x==0) $ inc c) xs
   readSTRef c
 where
   inc c = modifySTRef' c (+1)

main :: IO ()
main = do
  print $ countZeros $ replicate 1000 0
  print $ countZerosST $ replicate 1000 0
