import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)
import Control.Applicative

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "specify number"
        (num:_) -> let
            n = read num in do
                conts <- BC.lines <$> BC.getContents
                mapM_ BC.putStrLn . drop (length conts - n) $ conts
