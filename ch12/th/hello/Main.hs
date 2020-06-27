{-# LANGUAGE TemplateHaskell #-}
module Main where

import Hello

main :: IO ()
main = $hello
