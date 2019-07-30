{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb.Irregular where

import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.PT.Common

shallowI :: VStructure -> VStructure
shallowI v@(Pers c _ _ _ _) = case c of
  "fazer" -> case v of
    Pers _ _ t m@IPRS p@P1 -> Pers c (Root Irr "FAÇ") t m p
    Pers _ _ t m@IPRF p    -> Pers c (Root Irr "FIZ") t m p
    Pers _ _ t m@IPPF p    -> Pers c (Root Irr "FIZ") t m p
    Pers _ _ t m@SIPF p    -> Pers c (Root Irr "FIZ") t m p
    Pers _ _ t m@SFUT p    -> Pers c (Root Irr "FIZ") t m p
    _                      -> v
  _ -> v
shallowI v = v

toCompI v = case v of
  Pers c (Root Reg "DIZ")  E' IFUT p -> Comp c (Impr c (Root Irr "D") I' INF)  (Pers "haver" (Root Cmp "hav") E' IPRS p)
  Pers c (Root Reg "DIZ")  E' IFPR p -> Comp c (Impr c (Root Irr "D") I' INF)  (Pers "haver" (Root Cmp "hav") E' IIPF p)
  Pers c (Root Reg "FAZ")  E' IFUT p -> Comp c (Impr c (Root Irr "F") A' INF)  (Pers "haver" (Root Cmp "hav") E' IPRS p)
  Pers c (Root Reg "FAZ")  E' IFPR p -> Comp c (Impr c (Root Irr "F") A' INF)  (Pers "haver" (Root Cmp "hav") E' IIPF p)
  Pers c (Root Reg "TRAZ") E' IFUT p -> Comp c (Impr c (Root Irr "TR") A' INF) (Pers "haver" (Root Cmp "hav") E' IPRS p)
  Pers c (Root Reg "TRAZ") E' IFPR p -> Comp c (Impr c (Root Irr "TR") A' INF) (Pers "haver" (Root Cmp "hav") E' IIPF p)
  _ -> v
