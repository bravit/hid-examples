import Data.Foldable (traverse_)
import Control.Monad
import Control.Monad.ST
import Data.STRef

comp1 :: ST s (STRef s Int)
comp1 = do
  -- ...
  newSTRef 0

comp2 :: STRef s Int -> ST s Int
comp2 ref = do
  -- ...
  readSTRef ref


--main = print $ runST (comp2 (runST comp1))

countZeros :: [Int] -> Int
countZeros = length . filter (== 0)

countZerosST :: [Int] -> Int
countZerosST xs = runST $ do
   c <- newSTRef 0
   traverse_ (\x -> when (x==0) $ inc c) xs
   readSTRef c
 where
   inc c = modifySTRef' c (+1)

main = do
  print $ runST (comp1 >>= comp2)
  print $ countZeros $ replicate 1000 0
  print $ countZerosST $ replicate 1000 0
