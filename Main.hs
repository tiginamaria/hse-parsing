module Main where

import Parser

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

main :: IO ()
main = do
  runParser " var = 1 - 2 + c - 30 "
  runParser " (((9)))"
  runParser " 1 * 2 - 3 / 4 + 5"
