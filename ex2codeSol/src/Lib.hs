module Lib
    ( splitOn
    , lex
    ) where

import Prelude hiding (lex)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch lst = let strip = dropWhile (==ch) lst
                 in case strip of
                    []     -> []
                    (x:xs) -> n : (splitOn ch b)
                                where
                              (n, b) = break (==ch) strip

lex :: String -> [String]
lex lst = splitOn ' ' lst
