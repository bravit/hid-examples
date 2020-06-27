{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module GenericSQL where

import GHC.Generics
import Data.Text (Text)
import TextShow

{-
INSERT INTO table_name (column1, column2, column3, ...)
VALUES (value1, value2, value3, ...);
-}

class ToSQL a where
  insertInto :: Text -> a -> Text

  default insertInto :: (Generic a, ToColumnsValuesLists (Rep a)) =>
                   Text -> a -> Text
  insertInto = insertIntoDefault

buildersToList :: [Builder] -> Builder
buildersToList [] = "()"
buildersToList (x:xs) = singleton '(' <> x <> go xs -- "(..
  where
    go (y:ys) = showbCommaSpace <> y <> go ys       -- .., ..
    go []     = singleton ')'                       -- ..)"

insertIntoDefault :: (Generic a, ToColumnsValuesLists (Rep a)) =>
                   Text -> a -> Text
insertIntoDefault table val =
  toText $ "INSERT INTO " <> fromText table <> " "
           <> buildersToList columns
           <> " VALUES " <> buildersToList values
  where
    (columns, values) = toColumnsValues (from val)

class ToColumnsValuesLists f where
  toColumnsValues :: f a -> ([Builder], [Builder])

instance ToColumnsValuesLists U1 where
  toColumnsValues _ = ([], [])

instance (ToColumnsValuesLists a, ToColumnsValuesLists b) =>
                ToColumnsValuesLists (a :*: b) where
  toColumnsValues (a :*: b) = (columns1 <> columns2, values1 <> values2)
    where
      (columns1, values1) = toColumnsValues a
      (columns2, values2) = toColumnsValues b

instance (ToColumnsValuesLists a) =>
               ToColumnsValuesLists (M1 i c a) where
  toColumnsValues (M1 a) = toColumnsValues a

instance {-# OVERLAPPING #-} (ToColumnsValuesLists a, Selector c) =>
               ToColumnsValuesLists (M1 S c a) where
  toColumnsValues s@(M1 a) = (fromString (selName s) : columns, values)
    where
      (columns, values) = toColumnsValues a

instance TextShow a => ToColumnsValuesLists (K1 i a) where
  toColumnsValues (K1 a) = ([], [showb a])
