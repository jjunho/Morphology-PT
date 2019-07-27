{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Nominal2

type Text = T.Text

main :: IO ()
main = TIO.putStrLn $ tshow $ mkNounM "bolo"
