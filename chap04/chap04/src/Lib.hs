{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Lib.Morph

import Control.Arrow
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.List
import Data.Maybe

getVerbSurfaces :: [Morph] -> Set Text
getVerbSurfaces = S.fromList . map surface . filter isVerb

getVerbBases :: [Morph] -> Set Text
getVerbBases = S.fromList . map base . filter isVerb

getSa_Nouns :: [Morph] -> Set Text
getSa_Nouns = S.fromList . map base . filter isSa_Noun

getPnoQ :: [Morph] -> [(Morph, Morph, Morph)]
getPnoQ ms = filter isPnoQ $ zip3 ms (tail ms) (tail . tail $ ms)
    where
        isPnoQ (p, no, q) = isNoun p && (get surface no == Just "の") && isNoun q

getLongestNounChain :: [Morph] -> [Morph]
getLongestNounChain = reverse . getMorph . snd . foldl' walk (mempty, mempty)
    where
        walk :: (State, State) -> Morph -> (State, State)
        walk (cur, record)  m
            | isNoun m                         = (add m cur, record)
            | getLength cur > getLength record = (mempty, cur)
            | otherwise                        = (mempty, record)


newtype State = State ([Morph], Int)
getMorph (State (ms, _)) = ms
getLength (State (_, n)) = n

instance Monoid State where
    mempty = State ([], 0)
    mappend (State (ms,n)) (State (ms', n')) = State (ms ++ ms', n+n')
add :: Morph -> State -> State
add m (State (ms, n)) = State (m:ms, n+1)


getFrequencies :: [Morph] -> [(Text, Int)]
getFrequencies =
    map (head &&& length) . group . sort . mapMaybe getIt
    where
        -- I don't know when the base is "*".
        getIt m = case get base m of
                       (Just "*") -> get surface m
                       other -> other

