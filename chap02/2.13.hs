{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List
import System.Environment (getArgs)

mergeColumns :: [BLC.ByteString] -> BLC.ByteString
mergeColumns =
        BLC.unlines . map (BLC.intercalate "\t") . transpose . map BLC.words

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Specify a file!"
        fs ->
            mapM BLC.readFile fs >>= BLC.writeFile "merged.txt" . mergeColumns
