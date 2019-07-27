module NLP.Morphology.PT.Verb.Base where

import           NLP.Morphology.PT.Common

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

