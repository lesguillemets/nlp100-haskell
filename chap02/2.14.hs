import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Environment (getArgs)

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "specify number"
        (num:_) -> let
            n = read num in
                BLC.getContents >>= mapM_ BLC.putStrLn . take n . BLC.lines
