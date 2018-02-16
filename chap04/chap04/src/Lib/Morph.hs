{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib.Morph where

import Data.Text (Text, unpack)

data Morph = EOS
           | Morph { surface :: Text
                   , partOfSpeech :: Text
                   , partOfSpeech1 :: Maybe Text
                   , partOfSpeech2 :: Maybe Text
                   , inflection :: Maybe Text
                   , base :: Text
                   , reading :: Text
                   , extra :: Maybe Text } deriving (Show, Eq)

get :: (Morph -> a) -> Morph -> Maybe a
get f m = case m of
               EOS -> Nothing
               _ -> Just $ f m

isMorph :: Morph -> Bool
isMorph EOS = False
isMorph _ = True

isVerb :: Morph -> Bool
isVerb EOS = False
isVerb Morph {..} = partOfSpeech == "動詞"

isNoun :: Morph -> Bool
isNoun EOS = False
isNoun Morph {..} = partOfSpeech == "名詞"

isSa_Noun :: Morph -> Bool
isSa_Noun EOS = False
isSa_Noun Morph {..} = partOfSpeech1 == Just "サ変名詞"

class (Show a) => PrettyPrintable a where
    pp :: a -> String
    pp = show

instance PrettyPrintable Morph where
    pp EOS = "EOS"
    pp Morph {..} = unpack base

instance PrettyPrintable Text where
    pp = unpack
