{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb.Regular where

import qualified Data.Text                        as T
import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Verb.Base
import qualified NLP.Morphology.PT.Verb.Irregular as Irregular
import           NLP.Morphology.PT.Verb.Morphemes
import           NLP.Morphology.Txt

deepR :: VStructure -> [Morpheme]
deepR v = case v of
  Prs c r t    m@IPRS p@P1 -> [morph r, Z, morph m, allo2 p]
  Prs c r t    m@IPRF p@P6 -> [morph r, morph t, morph IPPF, allo1 p]
  Prs c r t    m@IPRF p    -> [morph r, morph t, morph m, allo1 p]
  Prs c r t@A' m@IIPF p    -> morphs v
  Prs c r t    m@IIPF p    -> [morph r, I, allo1 m, morph p]
  Prs c r A'   m@SPRS p    -> [morph r, E, morph m, morph p]
  Prs c r _    m@SPRS p    -> [morph r, A, morph m, morph p]
  Prs c r t    m@SFUT p@P5 -> [morph r, morph t, morph m, allo2 p]
  Prs c r t    m@INFP p@P5 -> [morph r, morph t, morph m, allo2 p]
  Prs c r t    m@IMPA p@P1 -> [Z]
  Prs c r t    m@IMPA p@P2 -> takeS $ deepR $ Prs c r t IPRS p
  Prs c r t    m@IMPA p@P5 -> takeS $ deepR $ Prs c r t IPRS p
  Prs c r t    m@IMPA p    -> deepR $ Prs c r t SPRS p
  Prs c r t    m@IMPN p@P1 -> [Z]
  Prs c r t    m@IMPN p    -> deepR $ Prs c r t SPRS p
  Prt c r E'   m@PRT  g n  -> [morph r, I, morph m, morph g, morph n]
  _                      -> morphs v

shallowR :: VStructure -> [Morpheme]
shallowR v = case Irregular.shallowI v of
  Prs c r t m p -> morphs v

takeS :: [Morpheme] -> [Morpheme]
takeS ms = case ms of
  [r, t, m, S]  -> [r, t, m, Z]
  [r, t, m, IS] -> [r, t, m, I]
