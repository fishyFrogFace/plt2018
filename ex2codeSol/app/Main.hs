module Main where

import Lib

parse :: String -> [Token]
parse = interpret . tokenize . Lib.lex

main :: IO ()
main = getLine >>= print . parse
    
