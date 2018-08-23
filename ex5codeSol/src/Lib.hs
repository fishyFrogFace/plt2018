module Lib
    ( fun
    , Maybe'(..)
    , safeFib
    , safeHead
    , showHead
    , fibOfHead
    ) where

fun = undefined

data Maybe' a = Thing a | NotAThing

-- TASK 1
-- Partial functions

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- complete the function "safeFib" that
-- returns Nothing if it's called with
-- a negative number
safeFib :: Int -> Maybe Int
safeFib n
    | n < 0     = Nothing
    | otherwise = Just (fib n)

-- create the function "safeHead" that takes
-- a list and returns the first element of
-- that list
-- if the list is empty, return Nothing
-- write an appropriate type signature
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- complete the function "showHead" that
-- takes a list and returns the String
-- "The first element is <x>" if the list
-- is not empty and the String "The list is
-- empty" if there is no first element
-- use "safeHead" to do this
showHead :: Show a => [a] -> String
showHead lst = case (safeHead lst) of
                Nothing -> "The list is empty"
                Just x  -> "The first element is " ++ show x

-- OPTIONAL EXERCISE
-- complete the function "fibOfHead"
-- that takes a list of integers, takes
-- the first element of the list, n
-- and returns the nth fibonacci number
-- you will need to combine "safeHead"
-- and safeFib
-- hint: (>>=) :: Monad m => m a -> (a -> m b) -> m b 
fibOfHead :: [Int] -> Maybe Int
fibOfHead lst = safeHead lst >>= safeFib

-- TASK 2
-- Using the type system

-- TASK 3
-- Maybe

-- TASK 4
-- Either
