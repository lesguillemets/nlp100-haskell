{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Monoid

main :: IO ()
main = do
    ptest morph sample1
    result <- parseFile "./neco.txt.mecab"
    case result of
         Left l -> print l
         Right r  -> print (length r)

-- morphEx = "ぬ\t助動詞,*,助動詞ぬ型,基本形,ぬ,ぬ,*\n"
--
sample1 = "吾輩\t名詞,普通名詞,*,*,吾輩,わがはい,代表表記:我が輩/わがはい カテゴリ:人\n"
