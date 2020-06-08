{-# LANGUAGE TemplateHaskell #-}
module DPLLSpec (spec) where

import Test.Hspec
import Literal
import Clause
import DPLL (addAssignment, isSat, isSingleLitClause, assignToSingleLits, dpll)

import Control.Lens
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Map.Strict as M

makeLenses ''Lit

spec :: Spec
spec = do
    describe "addAssignment" $ do
        it "assign" $
            let inputMap = M.fromList [(10, True), (11, False), (12, True), (13, True)]
                wantMap = M.fromList [(10, True), (11, False), (12, True), (13, True), (14, True)]
                l = (Lit 14 False)
            in addAssignment (Just inputMap) (l^.name) (getTrue l) `shouldBe` (Just wantMap)
    describe "isSat" $ do
        it "nothing" $
            isSat Nothing `shouldBe` False
        it "assignment" $
            let inputMap = M.fromList [(10, True), (11, False)]
            in isSat (Just inputMap) `shouldBe` True
    describe "isSingleLitClause" $ do
        it "single lit clauses" $
            let input = [[(Lit 10 True)],[(Lit 11 False)],[(Lit 12 True)]]
            in isSingleLitClause input `shouldBe` True
        it "multiple lit clauses" $
            let input = [[(Lit 10 True)],[(Lit 11 False),(Lit 12 True)],[(Lit 13 True)],[(Lit 14 False),(Lit 15 False),(Lit 16 True)]]
            in isSingleLitClause input `shouldBe` False
    describe "assignToSingleLits" $ do
        it "satisfiable" $
            let input = [[(Lit 10 True)], [(Lit 11 False)], [(Lit 12 True)]]
                assignments = assignToSingleLits (Just (M.fromList [])) input []
                assignList = M.toList (fromJust assignments)
            in assignList `shouldBe` [(10, False), (11, True), (12, False)]
        it "unsatisfiable" $
            let input = [[(Lit 10 True)], [(Lit 10 False)], [(Lit 11 True)]]
                assignments = assignToSingleLits (Just (M.fromList [])) input []
            in assignments `shouldBe` Nothing
    describe "dpll" $ do
        it "satisfiable" $
            let input = [[(Lit 1 False), (Lit 2 False), (Lit 3 False)],[(Lit 1 False), (Lit 2 False), (Lit 3 True)],[(Lit 1 False), (Lit 2 True), (Lit 3 False)],[(Lit 1 False), (Lit 2 True), (Lit 3 True)],[(Lit 1 True), (Lit 2 False), (Lit 3 False)],[(Lit 1 True), (Lit 2 False), (Lit 3 True)],[(Lit 1 True), (Lit 2 True), (Lit 3 False)]]
                assignments = (Just (M.fromList []))
                finalAssignments = dpll input assignments
            in finalAssignments `shouldBe` (Just (M.fromList [(1, True), (2, True), (3, True)]))
        it "unsatisfiable" $
            let input = [[(Lit 1 False), (Lit 2 False), (Lit 3 False)],[(Lit 1 False), (Lit 2 False), (Lit 3 True)],[(Lit 1 False), (Lit 2 True), (Lit 3 False)],[(Lit 1 False), (Lit 2 True), (Lit 3 True)],[(Lit 1 True), (Lit 2 False), (Lit 3 False)],[(Lit 1 True), (Lit 2 False), (Lit 3 True)],[(Lit 1 True), (Lit 2 True), (Lit 3 False)],[(Lit 1 True), (Lit 2 True), (Lit 3 True)]]
                assignments = (Just (M.fromList []))
                finalAssignments = dpll input assignments
            in finalAssignments `shouldBe` Nothing
