{-# LANGUAGE RankNTypes #-}

module NLP.Morphology.PT.Verb
  ( VStructure
  , impersonal
  , personal
  , participle
  )
  where

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.Txt
