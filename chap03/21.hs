import Text.Parsec
import Control.Monad
import Control.Applicative (liftA)

inside :: Parsec String u String
inside = many1 (noneOf "[]")

squareBr :: Parsec String u String
squareBr = char '[' *> (concat <$>
            manyTill (
                many1 (noneOf "[]")
                <|>
                (\s -> '[':s++"]") <$> squareBr
            ) (char ']'))

squares = many $ many (noneOf "[]") *> squareBr <* many (noneOf "[]")

doubleSquare :: Parsec String u String
doubleSquare = char '[' *> squareBr <* char ']'

doubleSquares :: Parsec String u [String]
doubleSquares = many $ many (noneOf "[]") *> doubleSquare <* many (noneOf "[]")

ds = many $ (many outside *> try doubleSquare <* many outside)

outside = many1 (noneOf "[]") <|> try singleSquare

singleSquare = char '[' *> many (noneOf "[]") <* char ']'

main = do
    parseTest squareBr "[[there]]isapen"
    parseTest ds "[[there]]is[and]x[[pen]]"
    parseTest ds "[[there]]is[[and]]x[[pen]]"
