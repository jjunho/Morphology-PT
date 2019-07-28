module NLP.Morphology.PT.Nominal.Base where

import NLP.Morphology.PT.Common

data NStructure
  = Nom { root          :: Root
        , thematicVowel :: ThematicVowel
        , gender        :: Gender
        , number        :: Number
        }
  | Pro { root          :: Root
        , thematicVowel :: ThematicVowel
        , person        :: Person
        , gender        :: Gender
        , number        :: Number
        }
  | Det { root          :: Root
        , thematicVowel :: ThematicVowel
        , gender        :: Gender
        , number        :: Number
        }
