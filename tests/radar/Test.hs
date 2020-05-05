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

randomsIO :: Random a => Int -> IO [a]
randomsIO n = replicateM n randomIO

randomTurns :: Int -> IO [Turn]
randomTurns = randomsIO

randomDirections :: Int -> IO [Direction]
randomDirections = randomsIO

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
   and [ rotateMany d ts == rotateMany' d ts | d <- every ]

test_orientRotateAgree :: [Direction] -> Bool
test_orientRotateAgree [] = True
test_orientRotateAgree ds@(d:_) = ds == rotateManySteps d (orientMany ds)

main :: IO ()
main = do
  ds <- randomDirections 1000
  ts <- randomTurns 1000
  when (not $ and [test_allTurnsInUse,
                   test_orientRotateAgree ds,
                    test_rotationsMonoidAgree ts])
       exitFailure
