{-# LANGUAGE OverloadedStrings #-}
module Lib.Parser where

import Lib.Morph

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text


parseFile :: FilePath -> IO (Either String [Morph])
parseFile f = parseOnly entries <$> TIO.readFile f

-- $ mecab -P
-- bos-feature: BOS/EOS,*,*,*,*,*,*
-- bos-format: 
-- cost-factor: 700
-- dicdir: /var/lib/mecab/dic/debian
-- dump-config: 1
-- eon-format: 
-- eos-format: EOS\n
-- eos-format-simple: EOS\n
-- eos-format-yomi: \n
-- eval-size: 4
-- lattice-level: 0
-- max-grouping-size: 24
-- nbest: 1
-- node-format: %m\t%H\n
-- node-format-simple: %m\t%F-[0,1,2,3]\n
-- node-format-yomi: %pS%f[5]
-- theta: 0.75
-- unk-eval-size: 2
-- unk-format: %m\t%H\n
-- unk-format-yomi: %M

entries :: Parser [Morph]
entries = many entry <* endOfInput

entry :: Parser Morph
entry = eos <|> morph

eos :: Parser Morph
eos = string "EOS" *> endOfLine *> return EOS

morph :: Parser Morph
morph =
    (Morph
        <$> (takeTill (== '\t') <* char '\t')
        <*> valC <*> tOrAstC <*> tOrAstC
        <*> tOrAstC <*> valC <*> valC <*> textOrAst) <* endOfLine

parseMorph = parseOnly morph

valC :: Parser Text
valC = takeWhile (liftA2 (&&) (/= separator) (/= '\n')) <* char separator

tOrAstC :: Parser (Maybe Text)
tOrAstC = textOrAst <* char separator

textOrAst :: Parser (Maybe Text)
textOrAst =
    (string "*" *> return Nothing)
    <|>
    (Just <$> takeWhile (liftA2 (&&) (/= separator) (/= '\n')))

separator :: Char
separator = ','
