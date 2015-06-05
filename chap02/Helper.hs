module Helper (mainWith, mainWithLazy) where
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Environment (getArgs)

mainWith:: (BC.ByteString -> BC.ByteString) -> IO ()
mainWith func = do
    args <- getArgs
    case args of
        [] -> putStrLn "Specify a file!"
        (f:_) -> BC.readFile f >>= BC.putStrLn . func

mainWithLazy :: (BLC.ByteString -> BLC.ByteString) -> IO ()
mainWithLazy func = do
    args <- getArgs
    case args of
        [] -> putStrLn "Specify a file!"
        (f:_) -> BLC.readFile f >>= BLC.putStrLn . func
