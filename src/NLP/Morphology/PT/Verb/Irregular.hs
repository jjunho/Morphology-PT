module NLP.Morphology.PT.Verb.Irregular where

import qualified Data.Text                        as T
import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.PT.Verb.Morphemes

shallowI :: VStructure -> VStructure
shallowI v = case v of
  _ -> v
