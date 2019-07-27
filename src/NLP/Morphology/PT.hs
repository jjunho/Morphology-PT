{-|
Module      : NLP.Morphology.PT
Description : Morphology for the Portuguese Language
Copyright   : (c) Juliano Paiva Junho, 2018-2019
License     : GPL-3
Maintainer  : jjunho@gmail.com
Stability   : experimental
Portability : POSIX

The library is built to be imported as follows:

> import qualified Data.Text         (Text)
> import qualified Data.Text         as T
> import qualified Data.Text.IO      as TIO
> import qualified NLP.Morphology.PT as PT

If you are using the library in ghci, don't forget to set `OverloadedStrings`:

> :set -XOverloadedStrings
-}

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
