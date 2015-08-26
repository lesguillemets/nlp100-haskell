import Text.Parsec
import Data.Either

entry :: Parsec String u [(String, String)]
entry = char '{' *> spaces *>
              pair `sepBy` (spaces >> char ',' >> spaces)
              <* spaces <* char '}' <* spaces

pair :: Parsec String u (String, String)
pair = do
    k <- jsonQuoted
    _ <- spaces >> char ':' >> spaces
    v <- jsonQuoted
    return (k,v)

dQuote = char '"'

jsonQuoted = dQuote *> quotedText <* dQuote

quotedText :: Parsec String u String
quotedText = concat <$> many (
                             many1 (noneOf "\"\\")
                            <|>
                             sequence [char '\\', anyChar]
                             )
    

main = do
    parseTest entry "{\"te\\\"xt\": \"hi\"}"
    readFile "./jawiki-country.json" >>=
        print . last . rights . map (parse entry "foo") . lines
