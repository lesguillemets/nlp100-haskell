import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import Data.List
import System.Environment (getArgs)

takeColumns :: Int -> BC.ByteString -> [BC.ByteString]
takeColumns n = take n .
    map BC.unlines . transpose . map (BC.split '\t') . BC.lines

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Specify a file!"
        (f:_) -> do
            [first,second] <- takeColumns 2 <$> BC.readFile f
            BC.writeFile "col0.txt" first
            BC.writeFile "col1.txt" second

