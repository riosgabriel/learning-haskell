module FPCourse.TagParser where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token

data Tag = MkTag String deriving Show

-- parseTag :: Parser Tag
-- parseTag =
--   do char '<'
--      x <- identifier
--      char '>'
--      return (MkTag x)

parseDiv :: Parser Tag
parseDiv = do
    string "<div>"
    return $ MkTag "div"

letterDigit :: Parser Char
letterDigit = letter <|> digit

bagOrBog :: Parser String
bagOrBog = try (string "bag") <|> string "bog"

varname :: Parser String
varname =
    do x <- letter
       xs <- many $ letter <|> digit
       return (x:xs)
