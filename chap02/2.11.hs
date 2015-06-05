import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)

expandTab :: BC.ByteString -> BC.ByteString
expandTab = BC.map (\c -> if c == '\t' then ' ' else c)

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Specify a file!"
        (f:_) -> BC.readFile f >>= BC.putStrLn . expandTab
