{-# LANGUAGE OverloadedStrings #-}

import TextShow
import Person

instance TextShow Person where
  showb (Person name Nothing) = fromString name
  showb (Person name (Just age)) =
    fromString name <> " (" <> showb age <> ")"

main = do
  printT homer
  printT spj
