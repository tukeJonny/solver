{-# LANGUAGE OverloadedStrings #-}
module Parser (
    parseDIMACSLines
  , DIMACSLine
  , isComment
  , literals
  , format, numLits, numClauses
    ) where

import Control.Applicative
import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take)

import Literal
import Clause

data DIMACSLine
    = Problem { format :: T.Text, numLits :: Int, numClauses :: Int }
    | Clause { literals :: Clause }
    | Comment { body :: T.Text } deriving (Show)

isComment :: DIMACSLine -> Bool
isComment (Comment _) = True
isComment _ = False

formatPrefix :: Parser T.Text
formatPrefix = do
    char 'p'
    space
    fmt <- takeTill (== ' ')
    space
    return fmt

problem :: Parser DIMACSLine
problem = Problem
    <$> formatPrefix
    <*> decimal <* space <*> decimal
    <* endOfLine

literal :: Parser Lit
literal = do
    sig <- string "-" <|> optional (string "+") *> return ""
    lit <- decimal
    char ' '
    case sig of
        "-" -> return (Lit lit True)
        otherwise -> return (Lit lit False)

clause :: Parser DIMACSLine
clause = Clause <$> many1 literal <* char '0' <* endOfLine

comment :: Parser DIMACSLine
comment = Comment <$ char 'c' <* space <*> takeText <* endOfLine

dimacsLine :: Parser DIMACSLine
dimacsLine = comment <|> problem <|> clause

dimacsLines :: Parser [DIMACSLine]
dimacsLines = many1 dimacsLine


parseDIMACSLines :: T.Text -> Either String [DIMACSLine]
parseDIMACSLines contents = parseOnly dimacsLines contents

