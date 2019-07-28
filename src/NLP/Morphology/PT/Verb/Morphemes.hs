{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module NLP.Morphology.PT.Verb.Morphemes where

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           NLP.Morphology.Txt
import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Verb.Base

data Morpheme
  = A
  | D
  | DES
  | E
  | I
  | IS
  | M
  | MOS
  | NDO
  | O
  | R
  | RA
  | S
  | SE
  | STE
  | STES
  | U
  | VA
  | Z
  | MRoot Text
  | Lit Text
  deriving (Show, Eq)

class Morph a where
  morph :: a -> Morpheme
  allo1 :: a -> Morpheme
  allo2 :: a -> Morpheme
  allo1 = morph
  allo2 = morph

instance Morph Root where
  morph (Root r) = MRoot r

instance Morph ThematicVowel where
  morph t = [A, E, I, O, U, Z] !! fromEnum t

instance Morph MoodTense where
  morph m = case m of
    IIPF -> VA
    IPPF -> RA
    SIPF -> SE
    SFUT -> R
    INFP -> R
    INF  -> R
    GER  -> NDO
    PRT  -> D
    _    -> Z

instance Morph PersonNumber where
  morph p = case p of
    P2 -> S
    P4 -> MOS
    P5 -> IS
    P6 -> M
    _  -> Z
  allo1 p = case p of
    P1 -> I
    P2 -> STE
    P3 -> U
    P5 -> STES
    _  -> morph p
  allo2 p = case p of
    P1 -> O
    P5 -> DES

instance Morph Gender where 
  morph MSC = O
  morph FEM = A

instance Morph Number where
  morph SG = Z
  morph PL = S

instance Txt Morpheme where
  txt m = case m of
    Z -> "∅"
    MRoot r -> T.toUpper r
    _ -> tshow m

morphs :: VStructure -> [Morpheme]
morphs v = case v of
  Imp r t m ->     [morph r, morph t, morph m]
  Prs r t m p ->   [morph r, morph t, morph m, morph p]
  Prt r t m g n -> [morph r, morph t, morph m, morph g, morph n]

instance Txt [Morpheme] where
  txt ms = T.intercalate "-" (txt <$> ms)
  -- txt = fix ((T.intercalate "-" .) . (<$>))