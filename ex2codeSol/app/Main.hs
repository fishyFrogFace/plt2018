module Main where

import Lib
import Control.Monad

parse :: String -> [Token]
parse = interpret . tokenize . Lib.lex

main :: IO ()
main = forever $ getLine >>= print . parse

mainInfix :: IO ()
mainInfix = undefined
