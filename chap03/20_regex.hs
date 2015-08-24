{-# LANGUAGE PackageImports #-}
-- こういう雰囲気で行けそうなのだが Unicode 周りで出鼻をくじかれたし
-- (http://torus.tumblr.com/post/118117701758)
-- どこ読んでも Parser 書くほうがいいでって書いてある
-- http://stackoverflow.com/q/14922579/#comment20937478_14922579
-- http://stackoverflow.com/a/6122766
-- http://d.hatena.ne.jp/kazu-yamamoto/20090309/1236590230
-- からそういう方向で行くことにする
import qualified Data.Text as T
import qualified Data.Text.IO as TI

import "regex-compat-tdfa" Text.Regex
import Text.Regex.TDFA

title :: String -> String
title t =  concat ["\"title\" *: *\"", t, "\""]

main = do
    readFile "./jawiki-country.json" >>=
        putStrLn . head . filter (=~ (title "イギリス")) . lines
