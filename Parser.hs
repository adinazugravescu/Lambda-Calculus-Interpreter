module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (result, rest) <- p input
        return (f result, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser pa) = Parser $ \input -> do
        (f, rest1) <- pf input
        (a, rest2) <- pa rest1
        return (f a, rest2)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser pa) <|> (Parser pb) = Parser $ \input -> case pa input of
        Nothing -> pb input
        justResult -> justResult

instance Monad Parser where
    (Parser pa) >>= f = Parser $ \input -> do
        (a, rest) <- pa input
        parse (f a) rest

-- Parsarea unui caracter specific
char :: Char -> Parser Char
char c = Parser $ \input -> case input of
    (x:xs) | x ==c      -> Just (x,xs)
    _ -> Nothing

-- Parsarea in functie de o conditie data
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser pred = Parser $ \input -> case input of
    (x:xs) | pred x     -> Just (x,xs)
    _ -> Nothing

-- Parsarea unui caracter dintr-o lista data
oneOf :: [Char] -> Parser Char
oneOf chars = predicateParser (`elem` chars)

-- Parsarea unui spatiu, tab si newline
space :: Parser Char
space = oneOf " \t\n"

-- Parsare zero sau mm spatii
spaces :: Parser String
spaces = many space

-- Parsarea unui sir -> variabila
variable :: Parser String
variable = some (oneOf['a'..'z'])

parseVar :: Parser Lambda
parseVar = Var <$> variable

parseAbs :: Parser Lambda
parseAbs = do
    char '\\'
    var <- variable
    char '.'
    expr <- parseLambdaExpr
    return $ Abs var expr

parseApp :: Parser Lambda
parseApp = do
    char '('
    spaces
    e1 <- parseLambdaExpr
    spaces
    e2 <- parseLambdaExpr
    spaces
    char ')'
    return $ App e1 e2

parseMacro :: Parser Lambda
parseMacro = Macro <$> some (oneOf(['A'..'Z'] ++ ['0'..'9']))

parseLambdaExpr :: Parser Lambda
parseLambdaExpr = parseVar <|> parseAbs <|> parseApp <|> parseMacro

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda input = case parse parseLambdaExpr input of
    Just(result, "") -> result
    _ -> error ("Invalid Lambda Expression")

-- Parsarea unei definitii de macro
parseBinding :: Parser Line
parseBinding = do
    name <- some (oneOf(['A'..'Z'] ++ ['0'..'9']))
    spaces
    char '='
    spaces 
    expr <- parseLambdaExpr
    return $ Binding name expr
    
-- 3.3.
parseLine :: String -> Either String Line
parseLine input = case parse parseBinding input of
    Just(result, "") -> Right result
    _ -> case parse (Eval <$> parseLambdaExpr) input of
        Just(result, "") -> Right result
        _ -> Left ("Invalid line of code")
 