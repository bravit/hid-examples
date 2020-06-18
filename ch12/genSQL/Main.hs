{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import GenericSQL

data Student = Student {studentId :: Int, name :: Text, year :: Int}
  deriving Generic
  deriving anyclass ToSQL

data Course = Course {courseId :: Int, title :: Text, instructor :: Text}
  deriving Generic
  deriving anyclass ToSQL

main :: IO ()
main = do
  TIO.putStrLn $ insertInto "students" (Student 18265 "John Doe" 2)
  TIO.putStrLn $ insertInto "courses" (Course 1 "Math" "Jane Doe")
