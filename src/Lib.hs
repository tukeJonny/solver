{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( solve
    , parseDIMACS
    , printResult
    , numLiterals
    ) where

import System.IO
import Data.List
import Data.Ord (comparing)
import qualified Data.Text as T
import Text.Printf (printf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Exception

import Literal
import Clause
import Parser
import DPLL

assignments :: Map Int Bool
assignments = M.fromList []

solve :: [Clause] -> Assignments
solve clauses = dpll clauses (Just assignments)

parseDIMACS :: T.Text -> Either String (DIMACSLine, [Clause])
parseDIMACS contents =
    case (parseDIMACSLines contents) of
        (Right dimacsLines) -> let lines = excludeComments dimacsLines
                                   problem = head lines
                                   clauses = map literals $ tail lines
                                in (Right (problem, clauses))
        (Left msg) -> (Left msg)

excludeComments :: [DIMACSLine] -> [DIMACSLine]
excludeComments lines = filter (\line -> not (isComment line)) lines

prettifyAssignments :: Map Int Bool -> T.Text
prettifyAssignments assignments =
    let assignList = M.toList assignments
        sortedAssignList = sortBy (comparing fst) assignList
        assignStrings = map (\(name, value) -> case value of
                                                True -> printf "%d" name
                                                otherwise -> printf "-%d" name) sortedAssignList
    in
        T.concat ["v ", (T.intercalate " " $ map T.pack assignStrings)]

printResult :: Assignments -> IO ()
printResult a =
    case a of
        (Just assignments) -> print (mconcat [ "s SATISFIABLE"
                                             , (prettifyAssignments assignments)
                                             , "\n"])
        Nothing            -> putStrLn "s UNSATISFIABLE"

numLiterals :: [Clause] -> Int
numLiterals clauses = let lits = clauses >>= id
                          absLits = map getLitName lits
                       in length $ nub absLits