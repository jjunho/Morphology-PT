{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module NLP.Morphology.PT.Verb (
    mkParadigm
  , getTense
  , mkVerb
  , Personal(..)
  , Impersonal(..)
  , Nominal(..)
) where

import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.PT.Verb.Regular
import           NLP.Morphology.Txt

mkParadigm :: Text -> [[VStructure]]
mkParadigm c = mconcat [ personalForms
                     , impersonalForms
                     , nominalForms
                     ]
  where
  impersonalForms = [[Impr c (mkRoot c) (getThematicVowel c) m ] | m <- bounds]
  personalForms   = [[toComp $ Pers c (mkRoot c) (getThematicVowel c) m p | p <- bounds ] | m <- bounds]
  nominalForms    = [[Nom c (mkRoot c) (getThematicVowel c) m g n | g <- bounds, n <- bounds ] | m <- bounds]

class GetTense a where
    getTense :: [[VStructure]] -> a -> [VStructure]

instance GetTense Personal where
    getTense p m = p !! fromEnum m

instance GetTense Impersonal where
    getTense p m = p !! ((fromEnum m) + length (bounds :: [Personal]))

instance GetTense Nominal where
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
mkVerb s = Verb s (deepV s) (shallowV s) (orthV s)
