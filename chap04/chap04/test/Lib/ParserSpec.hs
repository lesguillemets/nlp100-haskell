{-# LANGUAGE OverloadedStrings #-}
module Lib.ParserSpec where

import Lib.Morph
import Lib.Parser

import Test.Hspec
import Data.Attoparsec.Text
import Data.Either
import Data.Monoid

main = hspec spec

spec :: Spec
spec = do
    describe "valC" $ do
        it "parses text and comma" $
            parseOnly valC "there," `shouldBe` Right "there"
    describe "eos" $ do
        it "parses eos" $
            parseOnly eos "EOS\n" `shouldBe` Right EOS
        it "doesn't parse anything else" $
            parseOnly eos sample1 `shouldSatisfy` isLeft
    describe "morph" $ do
        it "parses normal morph entry" $
            parseOnly morph sample1 `shouldBe` Right result1
        it "parses normal morph entry : part 2" $
            parseOnly morph sample2 `shouldBe` Right result2

    describe "entry" $ do
        it "eos as eos" $
            parseOnly entry "EOS\n" `shouldBe` Right EOS
        it "parses normal morph entry" $
            parseOnly entry sample1 `shouldBe` Right result1
        it "parses normal morph entry : part 2" $
            parseOnly entry sample2 `shouldBe` Right result2

    describe "entries" $ do
        it "ex0" $
            parseOnly entries (sample1 <> sample2)
                `shouldBe` Right [result1, result2]
        it "ex0-rv" $
            parseOnly entries (sample2 <> sample1)
                `shouldBe` Right [result2, result1]


sample1 = "吾輩\t名詞,普通名詞,*,*,吾輩,わがはい,代表表記:我が輩/わがはい カテゴリ:人\n"
result1 :: Morph
result1 =
    Morph "吾輩" "名詞" (Just "普通名詞") Nothing Nothing
    "吾輩" "わがはい" (Just "代表表記:我が輩/わがはい カテゴリ:人")

sample2 = "。\t特殊,句点,*,*,。,。,*\n"
result2 :: Morph
result2 = Morph "。" "特殊" (Just "句点") Nothing Nothing
            "。" "。" Nothing
