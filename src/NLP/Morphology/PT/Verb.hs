{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb (
    mkParadigm
  , getTense
  , mkVerb
  , personal
  , impersonal
  , participle
  , Personal(..)
  , Impersonal(..)
  , NominalVerb(..)
) where

import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Verb.Base
import qualified NLP.Morphology.PT.Verb.Regular as Regular
import           NLP.Morphology.Txt

mkParadigm :: Text -> [[VStructure]]
mkParadigm c = mconcat [ personalForms
                       , impersonalForms
                       , participleForm
                       ]
  where
  impersonalForms = [[impersonal c m ] | m <- bounds]
  personalForms   = [[personal c m p | p <- bounds ] | m <- bounds]
  participleForm  = [[participle c m g n | g <- bounds, n <- bounds ] | m <- bounds]

impersonal :: Citation -> Impersonal -> VStructure
impersonal c = Impr c (mkRoot c) (getThematicVowel c)

personal :: Citation -> Personal -> Person -> VStructure
personal c m p = Regular.toComp $ Pers c (mkRoot c) (getThematicVowel c) m p

participle :: Citation -> NominalVerb -> Gender -> Number -> VStructure
participle c = Nom c (mkRoot c) (getThematicVowel c)

class GetTense a where
    getTense :: [[VStructure]] -> a -> [VStructure]

instance GetTense Personal where
    getTense p m = p !! fromEnum m

instance GetTense Impersonal where
    getTense p m = p !! (fromEnum m + length (bounds :: [Personal]))

instance GetTense NominalVerb where
    getTense p _ = last p

data Verb
  = Verb { structure :: VStructure
         , deep      :: [Morpheme]
         , shallow   :: [Morpheme]
         , orth      :: Text
         }
  deriving (Show, Eq)

instance Txt Verb where
  txt (Verb st d s o) = T.intercalate "\t" [txt st, txt d, txt s, txt o]

instance Txt [Verb] where
  txt ts = T.intercalate "\n" (fmap txt ts)

instance Txt [[Verb]] where
  txt ts = T.intercalate "\n\n" (fmap txt ts)

mkVerb :: VStructure -> Verb
mkVerb s = Verb s (Regular.deepR s) (Regular.shallowR s) (Regular.orth s)
