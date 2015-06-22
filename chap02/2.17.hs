import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)
import Control.Applicative
import qualified Data.Set as S

firstColumns :: [BC.ByteString] -> S.Set BC.ByteString
firstColumns = S.fromList . map (head . BC.split '\t')

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "specify number"
        (f:_) -> do
            keys <- firstColumns . BC.lines <$> BC.readFile f
            mapM_ BC.putStrLn . S.toAscList $ keys

