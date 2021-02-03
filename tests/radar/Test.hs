{-# LANGUAGE StandaloneDeriving #-}

import System.Exit (exitFailure)

import Data.List (sort, nub)
import Control.Monad (replicateM, when)
import System.Random
import System.Random.Stateful (uniformRM, uniformM)

import Radar

instance UniformRange Turn where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
    pure $ toEnum res

instance Uniform Turn where
  uniformM rng = uniformRM (minBound, maxBound) rng

instance UniformRange Direction where
  uniformRM (lo, hi) rng = do
    res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
    pure $ toEnum res

instance Uniform Direction where
  uniformM rng = uniformRM (minBound, maxBound) rng

uniformIO :: Uniform a => IO a
uniformIO = getStdRandom uniform

uniformsIO :: Uniform a => Int -> IO [a]
uniformsIO n = replicateM n uniformIO

randomTurns :: Int -> IO [Turn]
randomTurns = uniformsIO

randomDirections :: Int -> IO [Direction]
randomDirections = uniformsIO

writeRandomFile :: (Uniform a, Show a) =>
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
