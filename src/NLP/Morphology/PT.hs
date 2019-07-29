{-|
Module      : NLP.Morphology.PT
Description : Morphology for the Portuguese Language
Copyright   : (c) Juliano Paiva Junho, 2018-2019
License     : GPL-3
Maintainer  : jjunho@gmail.com
Stability   : experimental
Portability : POSIX

Haddock documentation: <https://jjunho.github.io/Morphology-PT>

If you are using only Portuguese:

> import qualified Data.Text         (Text)
> import qualified Data.Text         as T
> import qualified Data.Text.IO      as TIO
> import           NLP.Morphology.PT

After the implementation of other languages and if you are using multiple languages at a time:

> import qualified Data.Text         (Text)
> import qualified Data.Text         as T
> import qualified Data.Text.IO      as TIO
> import qualified NLP.Morphology.PT as PT
> import qualified NLP.Morphology.KR as KR
> import qualified NLP.Morphology.JA as JA

Don't forget to set `OverloadedStrings` and/or use the `LANGUAGE` pragma:

> :set -XOverloadedStrings
-}

module NLP.Morphology.PT (
  -- * Types
    Personal(..)
  , Impersonal(..)
  , Nominal(..)
  , Person(..)
  , Gender(..)
  , Number(..)

  -- * Verbs
  -- , impersonal
  -- -- | Creates an impersonal `VStructure` with the citation form of the verb.
  -- -- The `MoodTense` applicable is `INF` or `GER`
  -- --
  -- -- >>> impersonal "falar" INF
  -- -- Imp {root = Root "fal", thematicVowel = A', moodTense = INF}
  -- , personal
  -- -- | Creates a personal `VStructure` with the citation form of the verb.
  -- --
  -- -- >>> personal "falar" IPRS P1
  -- -- Prs {root = Root "fal", thematicVowel = A', moodTense = IPRS, personNumber = P1}
  -- , participle
  -- -- | Creates a participle `VStructure` with the citation form of the verb.
  -- --
  -- -- >>> participle "falar" MSC SG
  -- -- Prt {root = Root "fal", thematicVowel = A', moodTense = PRT, gender = MSC, number = SG}

  , mkParadigm
  , getTense
  -- * Nominals
  , noun
  , adjective
  , pronoun
  , determiner

  -- * Morphological transformations
  , deep
  , deepTxt
  , shallow
  , shallowTxt
  , orth

  -- * Formatting & IO
  , txt
  -- | Formats the data in order to print.
  --
  -- >>> txt $ personal "falar" IPRS P1
  -- "\8730FAL-A-IPRS-P1/SG"
  , putTxt
  -- | Formats and prints the data (no newline).
  --
  -- >>> putTxt $ personal "falar" IPRS P1
  -- √FAL-A-IPRS-P1/SG
  , putTxtLn
  -- | Formats the data in order to print (with newline).
  --
  -- >>> putTxtLn $ personal "falar" IPRS P1
  -- √FAL-A-IPRS-P1/SG
  , putParadigm
  ) where

import           Data.Text                 (Text)
import qualified Data.Text.IO              as TIO
import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Nominal
import           NLP.Morphology.PT.Verb
import           NLP.Morphology.Txt

putParadigm :: Text -> IO ()
putParadigm = TIO.putStrLn . txt . (mkVerb <$$>) . mkParadigm
