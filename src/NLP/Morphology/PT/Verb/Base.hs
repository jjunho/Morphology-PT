{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb.Base where

import qualified Data.Text                as T
import           NLP.Morphology.PT.Common
import           NLP.Morphology.Txt

type PersonNumber = Person

data MoodTense
  = IPRS
  | IPRF
  | IIPF
  | IPPF
  | IFUT
  | IFPR
  | SPRS
  | SIPF
  | SFUT
  | IMPA
  | IMPN
  | INFP
  | INF
  | GER
  | PRT
  deriving (Show, Eq, Enum, Bounded)

instance Txt MoodTense where
  txt = tshow

impersonal :: Citation -> MoodTense -> VStructure
impersonal c = Imp (toRoot c) (toThematicVowel c)

personal :: Citation -> MoodTense -> PersonNumber -> VStructure
personal c = Prs (toRoot c) (toThematicVowel c)

participle :: Citation -> Gender -> Number -> VStructure
participle c = Prt (toRoot c) (toThematicVowel c) PRT

data VStructure
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

instance Txt VStructure where
  txt (Prs (Root r) t m p) = T.intercalate "-" ["√" <> T.toUpper r, txt t, txt m, txt p]

toRoot :: Citation -> Root
toRoot = Root . T.init . T.init

toThematicVowel :: Citation -> ThematicVowel
toThematicVowel = toTV . T.takeEnd 1 . T.init
