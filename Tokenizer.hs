module Tokenizer where

data Token = TDigit Integer
           | TIdent String
           | TOp Operator
           | TLParen
           | TRParen
           | TAssign
           | TEof
           deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Mult
              | Div
              deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEof]
tokenize (c : cs) | isOperator c   = TOp (operator c) : tokenize cs
                  | isDigit c      =
                      let (num, cs') = span isDigit cs in
                      TDigit (number (c:num)) : tokenize cs'
                  | isAlpha c      = 
                      let (var, cs') = span isAlpha cs in
                      TIdent (c:var) : tokenize cs'
                  | c == '('       = TLParen : tokenize cs
                  | c == ')'       = TRParen : tokenize cs
                  | c == '='       = TAssign : tokenize cs
                  | isWhiteSpace c = tokenize cs
                  | otherwise = error ("Lexical error: unacceptable character " ++ [c])

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

digit :: Char -> Integer
digit c | c == '0' = 0
        | c == '1' = 1
        | c == '2' = 2
        | c == '3' = 3
        | c == '4' = 4
        | c == '5' = 5
        | c == '6' = 6
        | c == '7' = 7
        | c == '8' = 8
        | c == '9' = 9
digit c = error ("Lexical error: " ++ c : " is not a digit!")

number :: String -> Integer
number (d : ds) | ds == [] = digit d
                | otherwise = (digit d) * 10 + number ds

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

alpha :: Char -> Char
alpha c = c

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"
