-- module OneNine where

import System.Random
import Helper (shuffle)

typoglycemia :: RandomGen g => g -> String -> (String, g)
typoglycemia g str = let
    results = scanl (processWord . snd) ("", g) . words $ str
    in
        (unwords . map fst $ results, snd (last results))

processWord :: RandomGen g => g -> String -> (String, g)
processWord g [] = ([], g)
processWord g [a] = ([a], g)
processWord g (s:ss) = let
    (mid,lst) = splitAt (length ss - 1) ss
    (midShuffled, g') = shuffle g mid
    in
        (s: midShuffled ++ lst, g')

sampleText =  "I couldn't believe that I could actually understand " ++
    "what I was reading : the phenomenal power of the human mind ."

main = do
    g <- getStdGen
    putStrLn . fst $ typoglycemia g sampleText
