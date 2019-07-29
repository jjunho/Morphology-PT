{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb
  ( module NLP.Morphology.PT.Common
  , module NLP.Morphology.PT.Verb.Base
  , completeParadigm
  )
  where

import qualified Data.Text                        as T
import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Verb.Base
import qualified NLP.Morphology.PT.Verb.Irregular as Irregular
import qualified NLP.Morphology.PT.Verb.Regular   as Regular
import           NLP.Morphology.Txt

completeParadigm :: Citation -> [[VStructure]]
completeParadigm c =
  [[personal c m p | p <- range] | m <- personalMTs] <>
  [[impersonal c INF]] <>
  [[impersonal c GER]] <>
  [[participle c g n | g <- range, n <- range]]

personalMTs :: [MoodTense]
personalMTs = filter (not . (`elem` [INF, GER, PRT])) range

instance Deep VStructure where
  deep    = (txt <$>) . Regular.deepR
  deepTxt = T.intercalate "-" . deep
