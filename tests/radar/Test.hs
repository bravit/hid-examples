{-# LANGUAGE StandaloneDeriving #-}

import System.Exit (exitFailure)

import Data.List (sort, nub)
import Control.Monad (replicateM, when)
import System.Random

import Radar

instance Random Turn where
  randomR (lo, hi) g = (toEnum i, g')
    where (i, g') = randomR (fromEnum lo, fromEnum hi) g
  random = randomR (minBound, maxBound)

instance Random Direction where
  randomR (lo, hi) g = (toEnum i, g')
    where (i, g') = randomR (fromEnum lo, fromEnum hi) g
  random = randomR (minBound, maxBound)

nRandomsIO :: Random a => Int -> IO [a]
nRandomsIO n = replicateM n $ getStdRandom random

randomTurns :: Int -> IO [Turn]
randomTurns = nRandomsIO

randomDirections :: Int -> IO [Direction]
randomDirections = nRandomsIO

writeRandomFile :: (Random a, Show a) =>
                   Int -> (Int -> IO [a]) -> FilePath -> IO ()
writeRandomFile n gen fname = do
  xs <- gen n
  writeFile fname $ unlines $ map show xs

deriving instance Ord Turn

test_allTurnsInUse :: Bool
test_allTurnsInUse = sort (nub [ orient d1 d2 | d1 <- every, d2 <- every ])
                      == every

test_rotationsMonoidAgree :: [Turn] -> Bool
test_rotationsMonoidAgree ts =
  let t = mconcat ts
  in and [ rotateMany d ts == rotate t d | d <- every]

test_orientRotateAgree :: [Direction] -> Bool
test_orientRotateAgree ds@(d:_) = ds == rotateManySteps d (orientMany ds)

main :: IO ()
main = do
  ds <- randomDirections 1000
  ts <- randomTurns 1000
  when (not $ and [test_allTurnsInUse,
                   test_orientRotateAgree ds,
                    test_rotationsMonoidAgree ts])
       exitFailure
