module Parser (parse_expr, parse_code) where

import Data.Char
import Control.Monad
import Control.Applicative
import Expr

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    return x = Parser (\s -> Just (x, s))
    mp >>= f = Parser (\s -> case parse mp s of
                                Nothing -> Nothing
                                Just (x, s') -> parse (f x) s')

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

--- type declaration over ---

parse_fail :: Parser a
parse_fail = Parser (\s -> Nothing)

instance Alternative Parser where
    empty = parse_fail
    p1 <|> p2 = Parser (\s -> case parse p1 s of
                                Nothing -> parse p2 s
                                Just (x, s') -> Just (x, s'))

parse_char :: Char -> Parser Char
parse_char c = Parser (\s -> case s of
                                [] -> Nothing
                                x : xs -> if x == c then Just (x, xs) else Nothing)

parse_predicate :: (Char -> Bool) -> Parser Char
parse_predicate p = Parser (\s -> case s of
                                    [] -> Nothing
                                    x : xs -> if p x then Just (x, xs) else Nothing)

starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> return []

plusParser :: Parser a -> Parser [a]
plusParser p = do
                x <- p
                xs <- starParser p
                return (x : xs)

varParser :: Parser String
varParser = 
    do  x <- parse_predicate isAlpha
        xs <- starParser (parse_predicate isAlpha)
        return (x : xs)

varExprParser :: Parser Expr
varExprParser = do
                    x <- varParser
                    return $ Variable x

funcExprParser :: Parser Expr
funcExprParser = 
    do  parse_char '\\'
        x <- varParser
        parse_char '.'
        e <- bodyExprParser
        return $ Function x e

paranExprParser :: Parser Expr
paranExprParser = 
    do  parse_char '('
        e <- exprParser
        parse_char ')'
        return e

macroExprParser :: Parser Expr
macroExprParser = 
    do  parse_char '$'
        x <- parse_predicate isAlpha
        xs <- starParser (parse_predicate isAlpha)
        return $ Macro (x : xs)

spaceBeforeExpr :: Parser Expr
spaceBeforeExpr = 
    do  parse_char ' '
        e <- bodyExprParser
        return e

enumExprParser :: Parser Expr
enumExprParser = 
    do  first <- bodyExprParser
        expressions <- starParser spaceBeforeExpr
        return $ foldl Application first expressions

bodyExprParser :: Parser Expr
bodyExprParser = funcExprParser <|> paranExprParser <|> varExprParser <|> macroExprParser

-- <expr> ::= <enum>
-- <enum> ::= <body> <space_before_body> ... <space_before_body>
-- <body> ::= <func> | <paran> | <var> | <macro>
-- <func> ::= '\' <var> '.' <body>
-- <paran> ::= '(' <expr> ')'

exprParser :: Parser Expr
exprParser = enumExprParser

macroNameParser :: Parser String
macroNameParser = 
    do  xs <- plusParser (parse_predicate isAlpha)
        return xs

assignParser :: Parser Code
assignParser =
    do  name <- macroNameParser
        starParser (parse_predicate isSpace)
        parse_char '='
        starParser (parse_predicate isSpace)
        e <- exprParser
        return $ Assign name e

evaluateParser :: Parser Code
evaluateParser = do
                    expr <- exprParser
                    return $ Evaluate expr

codeParser :: Parser Code
codeParser = assignParser <|> evaluateParser

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr s = case parse exprParser s of
    Nothing -> Variable "error"
    Just (x, _) -> x

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code s = case parse codeParser s of
    Nothing -> Evaluate (Variable "error")
    Just (x, _) -> x
