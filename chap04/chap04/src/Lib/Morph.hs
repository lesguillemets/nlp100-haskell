{-# LANGUAGE OverloadedStrings #-}
module Lib.Morph where

import Data.Text (Text)

data Morph = EOS
           | Morph { surface :: Text
                   , partOfSpeech :: Text
                   , partOfSpeech1 :: Maybe Text
                   , partOfSpeech2 :: Maybe Text
                   , inflection :: Maybe Text
                   , dictForm :: Text
                   , reading :: Text
                   , extra :: Maybe Text } deriving (Show, Eq)
