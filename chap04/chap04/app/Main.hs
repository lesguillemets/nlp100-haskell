{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)

import Lib
import Lib.Morph
import Lib.Parser
import Data.Monoid
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import GHC.Exts (sortWith)

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
            3 -> mapM_ TIO.putStrLn . getSa_Nouns $ mps
            4 -> mapM_ (TIO.putStrLn .  f) . getPnoQ $ mps
            5 -> TIO.putStrLn . mconcat . map surface . getLongestNounChain $ mps
            6 -> mapM_ (TIO.putStrLn . g) . sortWith (negate . snd) . getFrequencies $ mps
    where
        f (x,y,z) = surface x <> surface y <> surface z
        g (t, n) = t <> "\t" <> (pack . show $ n)

