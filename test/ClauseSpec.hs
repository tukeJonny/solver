module ClauseSpec (spec) where

import Test.Hspec
import Literal
import Clause

spec :: Spec
spec = do
    describe "hasLit" $ do
        it "included" $
            hasLit (Lit 10 True) [
                (Lit 11 False), (Lit 12 True)
              , (Lit 10 False), (Lit 10 True)
            ] `shouldBe` True
        it "not included" $
            hasLit (Lit 10 True) [
                (Lit 11 False), (Lit 12 True)
              , (Lit 13 False), (Lit 10 False)
            ] `shouldBe` False
    describe "excludeInvLit" $ do
        it "can exclude" $
            excludeInvLit (Lit 10 True) [
                (Lit 11 False), (Lit 10 True)
              , (Lit 10 True), (Lit 10 True)
              , (Lit 10 True), (Lit 13 False)
            ] `shouldBe` [
                (Lit 11 False)
              , (Lit 13 False)
            ]
        it "can not exclude" $
            excludeInvLit (Lit 10 True) [
                (Lit 11 False), (Lit 13 True)
              , (Lit 11 True), (Lit 14 True)
              , (Lit 16 True), (Lit 13 False)
            ] `shouldBe` [
                (Lit 11 False), (Lit 13 True)
              , (Lit 11 True), (Lit 14 True)
              , (Lit 16 True), (Lit 13 False)
            ]
    describe "collectPureLits" $ do
        it "can filter" $
            (collectPureLits [
                [(Lit 10 True), (Lit 11 False)]
              , [(Lit 10 False), (Lit 11 True)]
              , [(Lit 13 True), (Lit 13 False)]
            ]) `shouldBe` []
        it "can not filter" $
            (collectPureLits [
                [(Lit 10 True), (Lit 10 True)]
              , [(Lit 11 True), (Lit 12 False)]
              , [(Lit 13 False), (Lit 14 True)]
            ]) `shouldBe` [
                (Lit 10 True), (Lit 10 True)
              , (Lit 11 True), (Lit 12 False)
              , (Lit 13 False), (Lit 14 True)
            ]