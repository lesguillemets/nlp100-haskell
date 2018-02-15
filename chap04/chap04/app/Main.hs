{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)

import Lib
import Lib.Morph
import Lib.Parser
import Data.Monoid
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    result <- parseFile "./neco.txt.mecab"
    ns <- map read <$> getArgs
    case result of
        Left _ -> print result
        Right rs -> mapM_ (run rs) ns


run :: [Morph] -> Int -> IO ()
run mps n = do
    putStrLn $ "Task " <> show n
    case n of
            0 -> putStrLn $ "Loaded " <> (show . length $ mps) <> "morphs"
            1 -> mapM_ TIO.putStrLn . getVerbSurfaces $ mps
            2 -> mapM_ TIO.putStrLn . getVerbBases $ mps

