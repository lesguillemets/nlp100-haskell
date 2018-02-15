{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib.Parser
import Data.Monoid

main :: IO ()
main = do
    result <- parseFile "./neco.txt.mecab"
    case result of
         Left l -> print l
         Right r  -> print (length r)
