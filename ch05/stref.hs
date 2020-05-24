import Control.Monad.ST
import Data.STRef

comp1 :: ST s (STRef s Int)
comp1 = newSTRef 42

comp2 :: STRef s Int -> ST s Int
comp2 ref = readSTRef ref

main :: IO ()
main = print $ runST (comp1 >>= comp2)
