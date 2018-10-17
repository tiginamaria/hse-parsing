 {-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Result a) where
  show (Success tree) = show tree
  show (Error err) = "Syntax error: " ++ err

main :: IO ()
main = do
  --runParser "[1-2-3] ++ x ++ [1, [12+1]]"
  --runParser "(((9)))"
  --runParser "1*2-3/4+5; 1^2 + 1"
  --runParser "!"
  --runParser "1 + -22"
  runParser "[]"
  runParser "[a]"
  runParser "[a] ++ [a]"
  runParser "[[abc=5], [], [5,6,[3^-2]]]"
