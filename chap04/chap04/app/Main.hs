{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Monoid

main :: IO ()
main = do
    pt "sample.txt"
--     result <- parseFile "./neco.txt.mecab"
--     case result of
--          Left l -> print l
--          Right r  -> print r

-- morphEx = "ぬ\t助動詞,*,助動詞ぬ型,基本形,ぬ,ぬ,*\n"
