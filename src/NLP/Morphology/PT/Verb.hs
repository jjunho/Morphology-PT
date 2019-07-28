{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb
  ( module NLP.Morphology.PT.Common
  , module NLP.Morphology.PT.Verb.Base
  , completeParadigm
  )
  where

import           NLP.Morphology.PT.Common
import           NLP.Morphology.PT.Verb.Base

completeParadigm :: Citation -> [[VStructure]]
completeParadigm c =
  [[personal c m p | p <- range] | m <- personalMTs] <>
  [[impersonal c INF]] <>
  [[impersonal c GER]] <>
  [[participle c g n | g <- range, n <- range]]

personalMTs :: [MoodTense]
personalMTs = filter (not . (`elem` [INF, GER, PRT])) range
