{-# LANGUAGE QuasiQuotes #-}
module Main where

import Str

verse :: String
verse = [str|What needs my Shakespeare for his honoured bones,
The labor of an age in pilèd stones,
Or that his hallowed relics should be hid
Under a star-ypointing pyramid?
Dear son of Memory, great heir of fame,
What need’st thou such weak witness of thy name?
Thou in our wonder and astonishment
Hast built thyself a live-long monument…|]

main :: IO ()
main = putStrLn verse
