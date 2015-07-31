import Data.Function
import Data.List
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString)
import System.Environment (getArgs)

sortWithNthColumn :: Int -> [ByteString] -> [ByteString]
sortWithNthColumn n = sortBy
    (flip compare `on` (read :: String -> Double)
        . BC.unpack . (!! n) . BC.split '\t')

sortFile :: FilePath -> IO ()
sortFile fName = BC.readFile fName >>=
    mapM_ BC.putStrLn . sortWithNthColumn 2 . BC.lines

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "specify number"
        (f:_) -> sortFile f
