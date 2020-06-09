module Main where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Lib

main :: IO ()
main =
    bracket (openFile "test.cnf" ReadMode) hClose $ \h -> do
        -- T.Textに変換して、パースを実施
        contents <- TIO.hGetContents h
        print "a"
        -- Eitherのエラーをけんさ
        -- 得られたProblemの行から、整合性検査
        -- solveを呼び出して、clausesがsatかunsatか判定し、結果を取得
        -- 結果が見つかった場合、satとして割り当てを出力
        -- 結果が見つからなかった場合、unsatとして出力
