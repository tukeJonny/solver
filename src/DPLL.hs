{-# LANGUAGE TemplateHaskell #-}
module DPLL where

import Literal
import Clause

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

makeLenses ''Lit

type Assignments = Maybe (Map Int Bool)

addAssignment :: Assignments -> Int -> Bool -> Assignments
addAssignment assignments k v = do
    m <- assignments
    Just (M.insert k v m)

isSat :: Assignments -> Bool
isSat Nothing = False
isSat (Just _) = True

isSingleLitClause :: [Clause] -> Bool
isSingleLitClause clauses = let isSingleLit = (\clause -> length clause == 1)
                            in
                                all isSingleLit clauses


assignToSingleLits :: Assignments -> [Clause] -> [Lit] -> Assignments
assignToSingleLits assignments [] _ = assignments
assignToSingleLits assignments clauses seenLits = let clause = head clauses
                                                      lit = head clause
                                                      foundOppositeLits = filter (isOpposite lit) seenLits
                                                 in case (length foundOppositeLits) of
                                                    0 -> let newAssignments = addAssignment assignments (lit^.name) (getTrue lit)
                                                             newClauses = (tail clauses)
                                                             newSeenLits = (lit : seenLits)
                                                         in
                                                            assignToSingleLits newAssignments newClauses newSeenLits
                                                    otherwise -> Nothing


assignToUnitClauses :: Assignments -> [Clause] -> [Clause] -> ([Clause], Assignments)
assignToUnitClauses assignments clauses [] = (clauses, assignments)
assignToUnitClauses assignments clauses unitClauses = let lit = (head (head unitClauses))
                                                          assignedClauses = tryAssign lit clauses
                                                          newAssignments = addAssignment assignments (lit^.name) (getTrue lit)
                                                      in assignToUnitClauses newAssignments assignedClauses (tail unitClauses)

collectUnitClauses :: [Clause] -> [Clause]
collectUnitClauses clauses = filter (\clause -> length clause == 1) clauses

assignToPureLits :: Assignments -> [Clause] -> [Lit] -> ([Clause], Assignments)
assignToPureLits assignments clauses [] = (clauses, assignments)
assignToPureLits assignments clauses pureLits = let pureLit = head pureLits
                                                    assignedClauses = tryAssign pureLit clauses
                                                    newAssignments = addAssignment assignments (pureLit^.name) (getTrue pureLit)
                                                in assignToPureLits newAssignments assignedClauses (tail pureLits)

dpll :: [Clause] -> Assignments -> Assignments
dpll clauses assignments = do
    if (isSingleLitClause clauses)
        then assignToSingleLits assignments clauses []
        else
            let unitClauses = collectUnitClauses clauses
                (clauses2, assignments2) = assignToUnitClauses assignments clauses unitClauses
                pureLits = collectPureLits clauses2
                (clauses3, assignments3) = assignToPureLits assignments clauses2 pureLits
            in case clauses3 of
                clauses
                  | length clauses == 0 -> assignments
                  | any (\clause -> length clause == 0) clauses -> Nothing
                  | otherwise ->
                    let lit = (head (head clauses3))
                        assignmentsTrue = addAssignment assignments3 (lit^.name) (getTrue lit)
                        assignmentsFalse = addAssignment assignments3 (lit^.name) (getFalse lit)
                        withTrue = dpll (tryAssign lit clauses3) assignmentsTrue
                        withFalse = dpll (tryAssign lit clauses3) assignmentsFalse
                    in case (withTrue, withFalse) of
                            ((Just assignments), _) -> (Just assignments)
                            (_, (Just assignments)) -> (Just assignments)
                            (_, _) -> Nothing
