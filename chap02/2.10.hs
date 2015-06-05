import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Monad (liftM)
import System.Environment (getArgs)

wc :: FilePath -> IO Int
wc = liftM (length . BLC.lines) . BLC.readFile
-- |
-- >>> wc "./2.10.hs"
-- 15

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Specify a file!"
        (f:_) -> wc f >>= print
