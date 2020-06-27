{-# LANGUAGE TemplateHaskell #-}
module Main where
import Predicates

data Complex = Polar Double Double | Rectangular Double Double

data Shape = Circle Double | Square Double
           | Triangle Double Double Double

$(mkPredicates ''Complex)
$(mkPredicates ''Shape)

main = mapM_ print [isPolar c1, isRectangular c2,
                    isCircle s1, isSquare s2, isTriangle s3]
  where
    c1 = Polar 5 10
    c2 = Rectangular 2 3
    s1 = Circle 4
    s2 = Square 10
    s3 = Triangle 1 1 1
