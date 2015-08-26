{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
import Text.Parsec
import Data.Either (rights)

type Level = Int
data Section = Section { _level :: Level, _text :: String}

instance Show Section where
    show (Section l t) = concat ["Section level ", show l, " : " , t]

sectPart :: Parsec String u Section
sectPart = do
    _ <- char '='
    m@Section{..} <- sectPart <|> (Section 0 <$> many (noneOf "="))
    _ <- char '='
    return $ m{_level = succ _level}

main = readFile "./uk.txt" >>=
    mapM_ print . rights . map (parse sectPart "") . lines
