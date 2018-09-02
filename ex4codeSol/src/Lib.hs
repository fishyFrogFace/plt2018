module Lib
    ( Token(..)
    , Op(..)
    , lex
    , tokenize
    , interpret
    , shunt
    ) where

import Prelude hiding (lex, dropWhile, takeWhile, break)
import Data.Char (isDigit)


-- Implement the following functions which respectively
-- return the elements at the beginning of a list satisfying
-- the given predicate; or drops them, returning the
-- remaining elements of the list.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p l@(x:xs)
  | p x       = dropWhile p xs
  | otherwise = l

-- Implement "break" which splits a list into two pieces,
-- one consisting of elements before a certain predicate is
-- satisfied, and then the remaining elements (including the
-- element satisfying the predicate).
break :: (a -> Bool) -> [a] -> ([a], [a])
break p l = (start, end)
  where start = takeWhile (not. p) l
        end = drop (length start) l

-- Implement "splitOn" which splits a list into non-empty
-- segments separated by a given character.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch lst = let strip = dropWhile (==ch) lst
                 in case strip of
                    []     -> []
                    (x:xs) -> n : (splitOn ch b)
                                where
                              (n, b) = break (==ch) strip


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
