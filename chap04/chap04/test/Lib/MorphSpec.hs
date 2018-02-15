{-# LANGUAGE OverloadedStrings #-}

module Lib.MorphSpec where

import Test.Hspec

import Lib.Morph
import Lib.Parser


spec :: Spec
spec = do
    describe "isVerb" $ do
        it "accepts verb" $
            isVerb resultVerb `shouldBe` True
        it "reject non-verb" $
            isVerb result1 `shouldBe` False
        it "reject non-verb" $
            isVerb result2 `shouldBe` False

result1 :: Morph
result1 =
    Morph "吾輩" "名詞" (Just "普通名詞") Nothing Nothing
    "吾輩" "わがはい" (Just "代表表記:我が輩/わがはい カテゴリ:人")

result2 :: Morph
result2 = Morph "。" "特殊" (Just "句点") Nothing Nothing
            "。" "。" Nothing

resultVerb :: Morph
resultVerb = Morph "見た" "動詞" Nothing (Just "母音動詞") (Just "タ形")
                    "見る" "みた" (Just "代表表記:見る/みる 補文ト 自他動詞:自:見える/みえる")
