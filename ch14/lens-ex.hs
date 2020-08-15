{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.Monoid

data Point = Point {_x :: Double, _y :: Double}
  deriving (Show, Eq)

data Segment = Segment {_beg :: Point, _end :: Point}
  deriving Show

makeLenses ''Point
makeLenses ''Segment

segs :: [Segment]
segs = [Segment (Point 0 0) (Point 100 0),
        Segment (Point 100 0) (Point 0 100),
        Segment (Point 0 100) (Point 0 0)]

move :: Double -> [Segment] -> [Segment]
move d = over (traverse . beg . x) (+d)
             . over (traverse . beg . y) (+d)
             . over (traverse . end . x) (+d)
             . over (traverse . end . y) (+d)

move' :: Double -> [Segment] -> [Segment]
move' d = compose [ over (traverse . point . coord) (+d)
                  | point <- [beg, end],
                    coord <- [x, y]]
  where
    compose = appEndo . foldMap Endo

main :: IO ()
main = print $ move' 100 segs
