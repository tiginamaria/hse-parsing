module Tokenizer where
import Prelude hiding (isSpace)

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Pow
              deriving (Show, Eq)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Pow
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

num :: String -> Integer
num [] = error ("Lexical error: is not a number!")
num s = read s :: Integer

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

alpha :: Char -> Char
alpha c = c

ident :: String -> String
ident s = s

isSpace :: Char -> Bool
isSpace c = c `elem` [' ', '\t', '\n', '\r', '\f', '\v']

notEmpty :: String -> Bool
notEmpty s = (/= []) s

skipSpaces :: String -> String
skipSpaces s = let (a, b) = span (isSpace) s in b
