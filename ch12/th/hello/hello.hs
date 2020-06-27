{-# LANGUAGE TemplateHaskell #-}
module Hello where
import Language.Haskell.TH

hello :: Q Exp
hello = [| putStrLn "Hello world" |]
