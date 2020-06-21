module Main where

import ServerUtils
import RemoteFunctions (registeredFunctions)

main :: IO ()
main = serveRPC "localhost" 1500 registeredFunctions

