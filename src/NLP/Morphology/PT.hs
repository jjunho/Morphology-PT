module NLP.Morphology.PT
  ( impersonal
  , personal
  , participle
  , noun
  , adjective
  , pronoun
  , determiner
  , txt
  , putTxt
  , putTxtLn
  ) where

import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Nominal (adjective, determiner, noun,
                                            pronoun)
import           NLP.Morphology.PT.Verb    (impersonal, participle, personal)
import           NLP.Morphology.Txt
