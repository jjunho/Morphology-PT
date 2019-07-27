{-# LANGUAGE RankNTypes #-}

module NLP.Morphology.PT.Verb
  ( impersonal
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

impersonal = undefined

personal = undefined

participle = undefined

data Structure
  = Imp { root          :: Root
        , thematicVowel :: ThematicVowel
        , moodTense     :: MoodTense
        }
  | Prs { root          :: Root
        , thematicVowel :: ThematicVowel
        , moodTense     :: MoodTense
        , personNumber  :: PersonNumber
  }
  | Prt { root          :: Root
        , thematicVowel :: ThematicVowel
        , moodTense     :: MoodTense
        , gender        :: Gender
        , number        :: Number
        }
  deriving (Show, Eq)

instance Txt Structure where
  txt = tshow
