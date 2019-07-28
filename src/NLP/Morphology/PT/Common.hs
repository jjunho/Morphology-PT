{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Common where

import           Data.Text          (Text)
import qualified Data.Text          as T
import           NLP.Morphology.Txt

type Citation = Text

data Gender
  = MSC
  | FEM
  deriving (Show, Eq, Enum, Bounded)

instance Txt Gender where
  txt = tshow

data Number
  = SG
  | PL
  deriving (Show, Eq, Enum, Bounded)

instance Txt Number where
  txt = tshow

data GenderNumber
  = MS
  | MP
  | FS
  | FP
  deriving (Show, Eq, Enum, Bounded)

data Person
  = P1
  | P2
  | P3
  | P4
  | P5
  | P6
  deriving (Show, Eq, Enum, Bounded)

instance Txt Person where
  txt p6 = (\(p, n) -> T.intercalate "/" [tshow p, tshow n]) $ fromP6 p6

data Root
  = Root Text
  deriving (Show, Eq)

instance Txt Root where
  txt (Root r) = case r of
    "" -> "∅"
    _  -> T.toUpper r

data Affix
  = Prefix Text
  | Suffix Text
  deriving (Show, Eq)

data ThematicVowel
  = A'
  | E'
  | I'
  | O'
  | U'
  | Z'
  deriving (Show, Eq, Enum, Bounded)

instance Txt ThematicVowel where
  txt tv = case tv of
    Z' -> "∅"
    _  -> T.init $ tshow tv

toTV :: Text -> ThematicVowel
toTV x = case x of
  "a" -> A'
  "e" -> E'
  "i" -> I'
  "o" -> O'
  "u" -> U'
  ""  -> Z'

toGN :: (Gender, Number) -> GenderNumber
toGN (g, n) = case (g, n) of
  (MSC, SG) -> MS
  (MSC, PL) -> MP
  (FEM, SG) -> FS
  (FEM, PL) -> FP

fromGN :: GenderNumber -> (Gender, Number)
fromGN gn = case gn of
  MS -> (MSC, SG)
  MP -> (MSC, PL)
  FS -> (FEM, SG)
  FP -> (FEM, PL)

fromP6 :: Person -> (Person, Number)
fromP6 p = case p of
  P4 -> (P1, PL)
  P5 -> (P2, PL)
  P6 -> (P3, PL)
  _  -> (p, SG)

toP6 :: (Person, Number) -> Person
toP6 (p, n) = case (p, n) of
  (P1, PL) -> P4
  (P2, PL) -> P5
  (P3, PL) -> P6
  (_, SG)  -> p

class Deep a where
  deep :: a -> [Text]
  deepTxt :: a -> Text

class Shallow a where
  shallow :: a -> [Text]
  shallowTxt :: a -> Text

class Orth a where
  orth :: a -> Text

range :: (Bounded a, Enum a) => [a]
range = [minBound .. maxBound]
