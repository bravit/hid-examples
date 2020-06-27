{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import GenericSQL

data Student = Student {studentId :: Int, name :: Text, year :: Int}
  deriving stock Generic
  deriving anyclass ToSQL

data Course = Course {courseId :: Int, title :: Text, instructor :: Text}
  deriving stock Generic
  deriving anyclass ToSQL

-- Doesn't compile:
{-
data Status = Ok | Err
  deriving stock Generic
  deriving anyclass ToSQL
-}

main :: IO ()
main = do
  TIO.putStrLn $ insertInto "students" (Student 18265 "John Doe" 2)
  TIO.putStrLn $ insertInto "courses" (Course 1 "Math" "Jane Doe")
