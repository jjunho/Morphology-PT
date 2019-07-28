{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb.Reg where

import qualified Data.Text                        as T
import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.PT.Verb.Morphemes
import           NLP.Morphology.Txt

instance Deep VStructure where
  deep    = (txt <$>) . deepV
  deepTxt = T.intercalate "-" . deep

deepV :: VStructure -> [Morpheme]
deepV v = case v of
  Prs r t    m@IPRS p@P1 -> [morph r, Z, morph m, allo2 p]
  Prs r t    m@IPRF p@P6 -> [morph r, morph t, morph IPPF, allo1 p]
  Prs r t    m@IPRF p    -> [morph r, morph t, morph m, allo1 p]
  Prs r t@A' m@IIPF p    -> morphs v
  Prs r t    m@IIPF p    -> [morph r, I, allo1 m, morph p]
  Prs r A'   m@SPRS p    -> [morph r, E, morph m, morph p]
  Prs r _    m@SPRS p    -> [morph r, A, morph m, morph p]
  Prs r t    m@SFUT p@P5 -> [morph r, morph t, morph m, allo2 p]
  Prs r t    m@INFP p@P5 -> [morph r, morph t, morph m, allo2 p]
  Prs r t    m@IMPA p@P1 -> [Z]
  Prs r t    m@IMPA p@P2 -> takeS $ deepV $ Prs r t IPRS p
  Prs r t    m@IMPA p@P5 -> takeS $ deepV $ Prs r t IPRS p
  Prs r t    m@IMPA p    -> deepV $ Prs r t SPRS p
  Prs r t    m@IMPN p@P1 -> [Z]
  Prs r t    m@IMPN p    -> deepV $ Prs r t SPRS p
  Prt r E'   m@PRT  g n  -> [morph r, I, morph m, morph g, morph n]
  _                      -> morphs v

takeS :: [Morpheme] -> [Morpheme]
takeS ms = case ms of
  [r, t, m, S]  -> [r, t, m, Z]
  [r, t, m, IS] -> [r, t, m, I]
