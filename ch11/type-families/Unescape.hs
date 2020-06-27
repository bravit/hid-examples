{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Unescape where

import GHC.Show (showLitChar)
import Unsafe.Coerce (unsafeCoerce)

newtype UnescapingChar = UnescapingChar {unescapingChar :: Char}

type family ToUnescapingTF (a :: k) :: k where
  ToUnescapingTF Char = UnescapingChar
  ToUnescapingTF (t b :: k) = (ToUnescapingTF t) (ToUnescapingTF b)
  ToUnescapingTF a = a

class ToUnescaping a where
    toUnescaping :: a -> ToUnescapingTF a

instance Show a => ToUnescaping a where
    toUnescaping = unsafeCoerce

type UnescapingShow t = (ToUnescaping t, Show (ToUnescapingTF t))

ushow :: UnescapingShow t => t -> String
ushow = show . toUnescaping

uprint :: UnescapingShow t => t -> IO ()
uprint = putStrLn . ushow

--------------------------------------------------------------------------

instance  Show UnescapingChar  where
    showsPrec _ (UnescapingChar '\'') = showString "'\\''"
    showsPrec _ (UnescapingChar c)    =
      showChar '\'' . showLitChar' c . showChar '\''

    showList cs = showChar '"' . showLitString' (map unescapingChar cs)
                . showChar '"'

showLitChar' :: Char -> ShowS
showLitChar' c s | c > '\DEL' =  showChar c s
showLitChar' c s = showLitChar c s

showLitString' :: String -> ShowS
showLitString' []         s = s
showLitString' ('"' : cs) s = showString "\\\"" (showLitString' cs s)
showLitString' (c   : cs) s = showLitChar' c (showLitString' cs s)

--------------------------------------------------------------------------

mch :: ToUnescapingTF (Maybe Char)
mch = toUnescaping $ Just 'Ж'

ech :: ToUnescapingTF (Either Char Char)
ech = toUnescaping $ (Left 'Ж' :: Either Char Char)

hello :: ToUnescapingTF (Maybe String, Char, Char, Char,
                         Char, Char, Char, Char, Char,
                         Char, Char, Char, Char)
hello = toUnescaping (Just "Привет, ",'м','и','р',
                      '!','!','!','!','!','!','!','!','!')

data Name = Name String
  deriving Show

name :: ToUnescapingTF Name
name = toUnescaping $ Name "Иван"

pangrams :: [String]
pangrams = [ "Vogt Nyx: »Büß du ja zwölf Qirsch, Kämpe!«"
           , "Voix ambiguë d’un cœur qui au zéphyr préfère les jattes de kiwi"
           , "Ζαφείρι δέξου πάγκαλο, βαθῶν ψυχῆς τὸ σῆμα"
           , "Съешь ещё этих мягких французских булок, да выпей чаю"
           ]
