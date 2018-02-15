{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Lib.Morph

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

getVerbSurfaces :: [Morph] -> Set Text
getVerbSurfaces = S.fromList . map surface . filter isVerb

getVerbBases :: [Morph] -> Set Text
getVerbBases = S.fromList . map base . filter isVerb
