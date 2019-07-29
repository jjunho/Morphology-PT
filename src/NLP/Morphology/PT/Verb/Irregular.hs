{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb.Irregular where

import           NLP.Morphology.PT.Verb.Base

shallowI :: VStructure -> VStructure
shallowI v = case v of
  _ -> v

toCompI v = case v of
  Pers c (Root Reg "DIZ")  E' IFUT p -> Comp c (Impr c (Root Irr "D") I' INF)  (Pers "haver" (Root Cmp "hav") E' IPRS p)
  Pers c (Root Reg "DIZ")  E' IFPR p -> Comp c (Impr c (Root Irr "D") I' INF)  (Pers "haver" (Root Cmp "hav") E' IIPF p)
  Pers c (Root Reg "FAZ")  E' IFUT p -> Comp c (Impr c (Root Irr "F") A' INF)  (Pers "haver" (Root Cmp "hav") E' IPRS p)
  Pers c (Root Reg "FAZ")  E' IFPR p -> Comp c (Impr c (Root Irr "F") A' INF)  (Pers "haver" (Root Cmp "hav") E' IIPF p)
  Pers c (Root Reg "TRAZ") E' IFUT p -> Comp c (Impr c (Root Irr "TR") A' INF) (Pers "haver" (Root Cmp "hav") E' IPRS p)
  Pers c (Root Reg "TRAZ") E' IFPR p -> Comp c (Impr c (Root Irr "TR") A' INF) (Pers "haver" (Root Cmp "hav") E' IIPF p)
  _ -> v
