import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)
import Control.Applicative

cutEvery :: Int -> [a] -> [[a]]
cutEvery n [] = []
cutEvery n xs = let (pre,post) = splitAt n xs
                    in
                        pre: cutEvery n post

writeEach :: (Int -> FilePath) -> [BC.ByteString] -> IO ()
writeEach f = mapM_ (uncurry BC.writeFile) . zip (map f [0..])

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "specify number"
        (num:_) -> let
            n = read num in do
                conts <- BC.lines <$> BC.getContents
                writeEach ((++".cut.txt") . show)  . map BC.unlines .
                    cutEvery ((length conts +n-1) `div` n) $ conts
