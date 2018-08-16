module Lib
    ( Token(..)
    , Op(..)
    , lex
    , tokenize
    , interpret
    , shunt
    ) where

import Prelude hiding (lex)
import Data.Char (isDigit)

data Token = TokOp Op
           | TokInt Int
           | TokErr
           deriving (Eq)

instance Show Token where
    show (TokOp op)   = show op
    show (TokInt num) = show num
    show TokErr       = "Parser error"

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

interpret :: [Token] -> [Token]
interpret []       = []
interpret [TokErr] = [TokErr]
interpret lst      = foldl calc [] lst

prec :: Op -> Int
prec Flip  = 3
prec Dupl  = 3
prec Mult  = 2
prec Div   = 2
prec Plus  = 1
prec Minus = 1

opLeq :: Op -> Op -> Ordering
opLeq a b = compare (prec a) (prec b)

shunt :: [Token] -> [Token]
shunt [TokErr] = [TokErr]
shunt lst      = si [] [] lst
            where
        si ops out []                = out ++ ops
        si ops out (op@(TokOp _):xs) = si (op:ops) out xs
        si ops out (num:xs)          = si ops (num:out) xs
