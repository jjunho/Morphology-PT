{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb.Regular where

import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.PT.Verb.Irregular
import           NLP.Morphology.Txt

toComp :: VStructure -> VStructure
toComp tc = case toCompI tc of
  Pers c r t IFUT p -> Comp c (Impr c r t INF) (Pers "haver" (Root Cmp "hav") E' IPRS p)
  Pers c r t IFPR p -> Comp c (Impr c r t INF) (Pers "haver" (Root Cmp "hav") E' IIPF p)
  _ -> tc

deepV :: VStructure -> [Morpheme]
deepV d = case d of
  Pers c r@(Root Cmp "hav") tv mt@IPRS P1    -> [morph r, morph tv, allom mt, iprfm P1]
  Pers c r@(Root Cmp "hav") tv mt@IPRS P2    -> [morph r, morph A', allom mt, morph P2]
  Pers c r@(Root Cmp "hav") tv mt@IPRS P3    -> [morph r, morph A', allom mt, morph P3]
  Pers c r@(Root Cmp "hav") tv mt@IPRS P6    -> [morph r, morph A', allom mt, morph P6]
  Pers c r@(Root Cmp "hav") tv mt@IPRS pn    -> [morph r, morph tv, allom mt, morph pn]
  Pers c r@(Root Cmp "hav") tv mt@IIPF pn    -> [morph r, I, A, morph pn]
  Pers c r                  tv mt@IPRF pn@P6 -> [morph r, morph tv, allom mt, iprfm pn]
  Pers c r                  tv mt@IPRF pn    -> [morph r, morph tv, morph mt, iprfm pn]
  Pers c r                  A' mt@IIPF pn    -> [morph r, A,        morph mt, morph pn]
  Pers c r                  _  mt@IIPF pn    -> [morph r, I,        A,        morph pn]
  Pers c r                  tv mt      pn    -> [morph r, morph tv, morph mt, morph pn]
  Impr c r                  tv mt            -> [morph r, morph tv, morph mt]
  Nom  c r                  tv mt      g n   -> [morph r, morph tv, morph mt, morph g, morph n]
  Comp c v1                 v2               -> deepV v1 <> deepV v2

shallowV :: VStructure -> [Morpheme]
shallowV s = case s of
  Pers c (Root Cmp "hav") E' IPRS pn@P1 -> deepV s
  Pers c (Root Cmp "hav") E' IIPF P5 -> [L "h", I, E, IS]
  Pers c (Root Cmp "hav") E' IIPF _ -> deepV s
  Pers c r A' mt@IPRS pn@P1 -> [morph r, Z,        morph mt, allom pn]
  Pers c r _  mt@IPRS pn@P1 -> [allom r, Z,        morph mt, allom pn]
  Pers c r I' mt@IPRS pn@P4 -> [morph r, I,        morph mt, morph pn]
  Pers c r I' mt@IPRS pn@P5 -> [morph r, Z,        morph mt, morph pn]
  Pers c r I' mt@IPRS pn    -> [morph r, E,        morph mt, morph pn]
  Pers c r A' mt@IPRF pn@P1 -> [allom r, E,        morph mt, iprfm pn]
  Pers c r A' mt@IPRF pn@P3 -> [morph r, O,        morph mt, iprfm pn]
  Pers c r tv mt@IPRF pn@P1 -> [morph r, Z,        morph mt, iprfm pn]
  Pers c r A' mt@IIPF pn@P5 -> [morph r, A,        allom mt, morph pn]
  Pers c r _  mt@IIPF pn@P5 -> [morph r, I,        E,        morph pn]
  Pers c r tv mt@IPPF pn@P5 -> [morph r, morph tv, allom mt, morph pn]
  Pers c r A' mt@SPRS pn    -> [allom r, E,        morph mt, morph pn]
  Pers c r _  mt@SPRS pn    -> [allom r, A,        morph mt, morph pn]
  Pers c r tv mt@SFUT pn@P5 -> [morph r, morph tv, morph mt, allom pn]
  Pers c r tv mt@INFP pn@P5 -> [morph r, morph tv, morph mt, allom pn]
  Pers c r tv mt@IMPA pn@P1 -> [L "-"]
  Pers c r tv mt@IMPA pn@P2 -> minusS $ shallowV (Pers c r tv IPRS P2)
  Pers c r tv mt@IMPA pn@P5 -> minusS $ shallowV (Pers c r tv IPRS P5)
  Pers c r tv mt@IMPA pn@P3 -> shallowV (Pers c r tv SPRS P3)
  Pers c r tv mt@IMPA pn@P4 -> shallowV (Pers c r tv SPRS P4)
  Pers c r tv mt@IMPA pn@P6 -> shallowV (Pers c r tv SPRS P6)
  Pers c r tv mt@IMPN pn@P1 -> [L "-"]
  Pers c r tv mt@IMPN pn    -> shallowV (Pers c r tv SPRS pn)
  Nom  c r E' mt      g   n -> [morph r, I, morph mt, morph g, morph n]
  Comp c s1 s2 -> shallowV s1 <> shallowV s2
  _ -> deepV s

minusS :: [Morpheme] -> [Morpheme]
minusS [r, t, m, S]  = [r, t, m, Z]
minusS [r, t, m, IS] = [r, t, m, I]

orthV :: VStructure -> Text
orthV o = case o of
  Pers c (Root Cmp "hav") E' mt@IPRS P2 -> oo [L "ás"]
  Pers c (Root Cmp "hav") E' mt@IPRS P3 -> oo [L "á"]
  Pers c (Root Cmp "hav") E' mt@IPRS P6 -> oo [L "ão"]
  Pers c (Root Cmp "hav") E' IIPF P5 -> oo [L "íeis"]
  Pers c (Root Cmp "hav") E' mt _ -> oo $ tail $ shallowV o
  Pers c r A' mt@IIPF pn@P4 -> oo [morph r, acute A, VA, morph pn]
  Pers c r A' mt@IIPF pn@P5 -> oo [morph r, acute A, VE, morph pn]
  Pers c r _  mt@IIPF pn@P4 -> oo [morph r, acute I, A, morph pn]
  Pers c r _  mt@IIPF pn@P5 -> oo [morph r, acute I, E, morph pn]
  Pers c r E' mt@IPPF pn@P4 -> oo [morph r, circ E, morph mt, morph pn]
  Pers c r E' mt@IPPF pn@P5 -> oo [morph r, circ E, allom mt, morph pn]
  Pers c r tv mt@IPPF pn@P4 -> oo [morph r, acute $ morph tv, morph mt, morph pn]
  Pers c r tv mt@IPPF pn@P5 -> oo [morph r, acute $ morph tv, allom mt, morph pn]
  Pers c r E' mt@SIPF pn@P4 -> oo [morph r, circ E, L "S", morph mt, morph pn]
  Pers c r E' mt@SIPF pn@P5 -> oo [morph r, circ E, L "S", morph mt, morph pn]
  Pers c r tv mt@SIPF pn@P4 -> oo [morph r, acute $ morph tv, L "S", morph mt, morph pn]
  Pers c r tv mt@SIPF pn@P5 -> oo [morph r, acute $ morph tv, L "S", morph mt, morph pn]
  Pers c r tv mt@SIPF pn    -> oo [morph r, morph tv, L "S", morph mt, morph pn]
  Pers c r tv mt@SFUT pn@P2 -> oo [morph r, morph tv, morph mt, LV, morph pn]
  Pers c r tv mt@SFUT pn@P6 -> oo [morph r, morph tv, morph mt, LV, morph pn]
  Pers c r tv mt@INFP pn@P2 -> oo [morph r, morph tv, morph mt, LV, morph pn]
  Pers c r tv mt@INFP pn@P6 -> oo [morph r, morph tv, morph mt, LV, morph pn]
  Pers c r tv mt@IMPN P1    -> oo $ shallowV o
  Pers c r tv mt@IMPN pn    -> "não " <> (oo $ shallowV o)
  Comp c s1 s2 -> orthV s1 <> orthV s2
  _ -> oo (shallowV o)
  where
    oo ms = T.toLower $ T.concat $ txt <$> (minus0 ms)

minus0 :: [Morpheme] -> [Morpheme]
minus0 = filter (Z /=)

acute :: Morpheme -> Morpheme
acute v = case v of
  A -> L "Á"
  E -> L "É"
  I -> L "Í"

circ :: Morpheme -> Morpheme
circ v = case v of
  E -> L "Ê"

root0 :: VStructure -> VStructure
root0 (Pers c (Root Cmp _) t m p) = Pers c (Root Cmp "") t m p

data Verb
  = Verb { structure :: VStructure
         , deep      :: [Morpheme]
         , shallow   :: [Morpheme]
         , orth      :: Text
         }
  deriving (Show, Eq)

instance Txt Verb where
  txt (Verb st d s o) = T.intercalate "\t" [txt st, txt d, txt s, txt o]

instance Txt [Verb] where
  txt ts = T.intercalate "\n" (fmap txt ts)

instance Txt [[Verb]] where
  txt ts = T.intercalate "\n\n" (fmap txt ts)

mkVerb :: VStructure -> Verb
mkVerb s = Verb s (deepV s) (shallowV s) (orthV s)
