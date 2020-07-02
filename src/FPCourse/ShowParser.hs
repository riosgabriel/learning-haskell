module FPCourse.ShowParser ( parseShow ) where

import           Data.List                              (intercalate)
import           Text.ParserCombinators.Parsec          hiding (runParser)
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

xmlHeader =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"


parseShow :: String -> String
parseShow = runParser showParser

showParser :: Parser String
showParser =
    listParser <|> -- [ ... ]
    tupleParser <|> -- ( ... )
    try recordParser <|> -- MkRec { ... }
    adtParser <|> -- MkADT ...
    number <|>    -- signed integer
    quotedString <?> "Parse error"

runParser :: Parser a -> String -> a
runParser p str = case parse p "" str of
    Left err  -> error $ "parse error at " ++ show err
    Right val -> val

openTag t = "<"++t++">"
closeTag t = "</"++t++">"
tag t v = concat [openTag t, v, closeTag t]

joinNL = intercalate "\n"

tagAttrs :: String -> [(String,String)] -> String -> String
tagAttrs t attrs v =
    concat [
        openTag (unwords $ [t] ++ (map (\(k,v) -> concat [k,"=\"",v,"\""]) attrs))
        ,v
        ,closeTag t
        ]

listParser = do
    ls <- brackets $ commaSep showParser
    return $ tag "list" $ joinNL $ map (tag "list-elt") ls

tupleParser = do
    ls <- parens $ commaSep showParser
    return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls

recordParser = do
    ti <- typeIdentifier
    ls <- braces $ commaSep kvParser
    return $ tagAttrs "record" [("name",ti)] (joinNL ls)

kvParser = do
    k <- identifier
    symbol "="
    t <- showParser
    return $ tagAttrs "elt" [("key",k)] t

typeIdentifier = do
    fst <- oneOf ['A' .. 'Z']
    rest <- many alphaNum
    whiteSpace
    return $ fst:rest

adtParser = tag "adt" <$> typeIdentifier

quotedString = do
    s <- stringLiteral
    return $ "\""++s++"\""

number = do
    n <- integer
    return $ show n

-- adtParser = do
--     ti <- type_identifier
--     return $ tag "adt" ti

lexer       = P.makeTokenParser emptyDef

parens          = P.parens lexer
brackets        = P.brackets lexer
braces          = P.braces lexer
commaSep        = P.commaSep lexer
whiteSpace      = P.whiteSpace lexer
symbol          = P.symbol lexer
identifier      = P.identifier lexer
integer         = P.integer lexer
stringLiteral   = P.stringLiteral lexer
