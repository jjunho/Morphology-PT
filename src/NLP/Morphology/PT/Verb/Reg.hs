module NLP.Morphology.PT.Verb.Reg where

import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.PT.Verb.Morphemes

deepV :: VStructure -> [Morpheme]
deepV v = case v of
  Prs r t IPRS P1 -> [morph r, morph t, Z, O]
  Prs r A' SPRS p -> [morph r, E, Z, morph p]
  Prs r _ SPRS p  -> [morph r, A, Z, morph p]
  _               -> morphs v
