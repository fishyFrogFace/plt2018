module Lib
    ( Token(..)
    , Op(..)
    , splitOn
    , lex
    , tokenize
    , interpret
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

data Op = Plus
        | Minus
        | Div
        | Mult
        | Dupl
        | Flip
        deriving (Show, Eq)

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
token "+"  = TokOp Plus
token "-"  = TokOp Minus
token "/"  = TokOp Div
token "*"  = TokOp Mult
token "#"  = TokOp Dupl
token "--" = TokOp Flip
token lst
    | isInt lst = TokInt (read lst)
    | otherwise = TokErr

tokenize :: [String] -> [Token]
tokenize lst = let tokens = map token lst
                in case (filter (== TokErr) tokens) of
                    [] -> tokens
                    _  -> [TokErr]            

calc :: [Token] -> Token -> [Token]
calc (TokInt x:TokInt y:xs) (TokOp Plus)  = TokInt (x + y):xs
calc (TokInt x:TokInt y:xs) (TokOp Minus) = TokInt (y - x):xs
calc (TokInt x:TokInt y:xs) (TokOp Mult)  = TokInt (x * y):xs
calc (TokInt x:TokInt y:xs) (TokOp Div)   = TokInt (y `div` x):xs
calc (tok:xs) (TokOp Dupl)                = tok:tok:xs
calc (TokInt x:xs) (TokOp Flip)           = TokInt (0 - x):xs
calc lst tok                              = tok:lst

--writing a folding function should be part of
--last exercise or this one
--at least talking about foldl/foldr and differences between them
interpret :: [Token] -> [Token]
interpret []       = []
interpret [TokErr] = [TokErr]
interpret lst      = foldl calc [] lst
