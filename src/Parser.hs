module Parser where

-- type Parser a = String -> a

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
 }

instance Functor Parser where
    fmap f p = Parser $ \s ->
        case runParser p s of
            Nothing        -> Nothing
            Just (a, rest) -> Just (f a, rest)

failure :: Parser a
failure = Parser $ const Nothing

entry :: a -> Parser a
entry a = Parser $ \s -> Just (a, s)

oneChar :: Parser Char
oneChar = Parser $ \s -> case s of
    []       -> Nothing
    (x : xs) -> Just (x, xs)

string :: String -> Parser String
string "" = entry ""
string (x:xs) = Parser $ \s ->
    case runParser oneChar s of
        Nothing -> Nothing
        Just (c, rest) ->
            if x == c
            then case runParser (string xs) rest of
                Nothing             -> Nothing
                Just (match, rest2) -> Just (c:match, rest2)
            else Nothing
