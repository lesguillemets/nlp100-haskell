{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (second)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString)
import Data.List
import Data.Function
import System.Environment (getArgs)

freqNthColumn :: Int -> [ByteString] -> M.Map ByteString Int
freqNthColumn n = foldl' (\dat entry -> M.insertWith (+) entry 1 dat) M.empty
                         . map ((!! n) . BC.split '\t')

getStats :: FilePath -> IO [(ByteString, Int)]
getStats f = sortBy (flip compare `on` snd ) . M.toList . freqNthColumn 0
    . BC.lines <$> BC.readFile f

concTup :: (ByteString,ByteString) -> ByteString
concTup (x,y) = BC.intercalate "\t" [x,y]
main = do
    args <- getArgs
    let f = case args of
                [] -> "./hightemp.txt"
                a -> head a
    getStats f >>= mapM_ (BC.putStrLn . concTup . second (BC.pack . show))

