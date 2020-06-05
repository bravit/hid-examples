{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text.IO as TIO

import EvalRPNExcept

rpns :: [Text]
rpns = ["answer",
        "12 13 + 1",
        "2 +",
        "x y +",
        "1x +",
        "1 22 1 22 0 2 * * * * *",
        "10 1 2 + 2 2 1 2 * + * * * 1 x 2 + + +"]

main :: IO ()
main = TIO.putStr $ evalRPNMany rpns [("answer", 42), ("x", 1)]
