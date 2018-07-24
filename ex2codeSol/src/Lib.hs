module Lib
    ( Token(..)
    , Op(..)
    , splitOn
    , lex
    , tokenize
    ) where

import Prelude hiding (lex)
import Data.Char (isDigit)

--TokErr is effectively a null type and won't be enforced by the type system
--the programmer will need to check for it
data Token = TokOp Op
           | TokInt Int
           | TokErr
           deriving (Eq)

instance Show Token where
    show (TokOp op)   = show op
    show (TokInt num) = show num
    show TokErr       = "Token Error"

data Op = Plus | Minus | Div | Mult deriving (Show, Eq)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch lst = let strip = dropWhile (==ch) lst
                 in case strip of
                    []     -> []
                    (x:xs) -> n : (splitOn ch b)
                                where
                              (n, b) = break (==ch) strip

isInt :: String -> Bool
isInt str
    | filter (==False) (map isDigit str) == [] = True
    | otherwise                                = False

lex :: String -> [String]
lex lst = splitOn ' ' lst

token :: String -> Token
token "+" = TokOp Plus
token "-" = TokOp Minus
token "/" = TokOp Div
token "*" = TokOp Mult
token lst
    | isInt lst = TokInt (read lst)
    | otherwise = TokErr

tokenize :: [String] -> [Token]
tokenize = map token
