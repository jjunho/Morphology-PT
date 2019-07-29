{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import qualified NLP.Morphology.PT  as PT
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  mapM_ (PT.putParadigm . T.pack) args
