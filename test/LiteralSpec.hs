module LiteralSpec (spec) where

import Test.Hspec
import Literal

spec :: Spec
spec = do
    describe "invert" $ do
        it "do invert" $
            invert (Lit 10 True) `shouldBe` (Lit 10 False)
        it "get same value by inverting twice" $
            invert (invert (Lit 10 True)) `shouldBe` (Lit 10 True)
    describe "isOpposite" $ do
        it "same" $
            isOpposite (Lit 10 True) (Lit 10 True) `shouldBe` False
        it "opposite" $
            isOpposite (Lit 10 True) (Lit 10 False) `shouldBe` True
    describe "getTrue&getFalse" $ do
        it "getTrue" $
            getTrue (Lit 10 True) `shouldBe` False
        it "getFalse" $
            getFalse (Lit 10 True) `shouldBe` True
