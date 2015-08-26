{-# LANGUAGE OverloadedStrings #-}
import Text.Parsec
import Data.Either
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TI
import qualified Data.Map as M
import Data.Map ((!))

entry :: Parsec Text u [(Text, Text)]
entry = char '{' *> spaces *>
              pair `sepBy` (spaces >> char ',' >> spaces)
              <* spaces <* char '}' <* spaces

pair :: Parsec Text u (Text, Text)
pair = do
    k <- jsonQuoted
    _ <- spaces >> char ':' >> spaces
    v <- jsonQuoted
    return (k,v)

dQuote = char '"'

jsonQuoted = dQuote *> quotedText <* dQuote

quotedText :: Parsec Text u Text
quotedText = T.concat . map T.pack <$> many (
                             many1 (noneOf "\"\\")
                            <|>
                             sequence [char '\\', anyChar]
                             )
    

-- text gives about x1.5 speed
main = TI.readFile "./jawiki-country.json" >>=
    TI.writeFile "./uk.json" . T.replace "\\n" "\n" .
    (! "text") . head . filter ((== "イギリス") . (! "title")) .
        map M.fromList . rights . map (parse entry "") . T.lines
