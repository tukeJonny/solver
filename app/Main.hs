{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.IO
import System.Exit
import System.Environment (getArgs)
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable

import Lib
import Parser (format, numLits, numClauses)

data SolverException =
    ParseError { msg :: String }
    | FileFormatError { fileFormat :: T.Text }
    | NumLitsError { gotLits :: Int }
    | NumClausesError { gotClauses :: Int }
    | UnknownError deriving (Show, Typeable)

instance Exception SolverException

supportedFileFormats :: [T.Text]
supportedFileFormats = ["cnf"]

main :: IO ()
main = do
    args <- getArgs
    bracket (openFile (args !! 0) ReadMode) hClose $ \h -> do
        contents <- TIO.hGetContents h
        case (parseDIMACS contents) of
            (Right (problem, clauses)) ->
                let gotFormat = (format problem)
                    gotNumLiterals = (numLiterals clauses)
                    gotNumClauses = (length clauses)
                    isValidFormat = gotFormat `elem` supportedFileFormats
                    isValidNumLits = gotNumLiterals == (numLits problem)
                    isValidNumClauses = gotNumClauses == (numClauses problem)
                in case (isValidFormat, isValidNumLits, isValidNumClauses) of
                    (True, True, True) ->
                        printResult $ solve clauses
                    (False, _, _) ->
                        throwIO (FileFormatError gotFormat)
                    (_, False, _) ->
                        throwIO (NumLitsError gotNumLiterals)
                    (_, _, False) ->
                        throwIO (NumClausesError gotNumClauses)
            (Left msg) ->
                throwIO (ParseError msg)
