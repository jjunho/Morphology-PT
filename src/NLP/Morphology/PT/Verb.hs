{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb (
    mkParadigm
  , putParadigm
  , getTense
  , Personal(..)
  , Impersonal(..)
  , Nominal(..)
) where

import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
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

putParadigm :: Text -> IO ()
putParadigm = TIO.putStrLn . txt . (mkVerb <$$>) . mkParadigm

class GetTense a where
    getTense :: [[VStructure]] -> a -> [VStructure]

instance GetTense Personal where
    getTense p m = p !! fromEnum m

instance GetTense Impersonal where
    getTense p m = p !! ((fromEnum m) + length (bounds :: [Personal]))

instance GetTense Nominal where
    getTense p _ = last p
