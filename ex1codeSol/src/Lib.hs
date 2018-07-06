module Lib
    ( add
    , fib
    , listOfEven
    , map'
    , safeFib
    , safeHead
    ) where

import Prelude hiding (map)

-- TASK 1
-- Simple functions

-- finish the function "add" that takes two integers
-- and returns the sum of them
add :: Int -> Int -> Int
add n m = n + m

-- TASK 2
-- Recursion

-- finish the function "fib" that calculates the
-- nth fibonnaci number 
-- assuming that 0th = 0 and 1st = 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- TASK 3
-- List comprehensions

-- create a list "listOfEven" which contains all 
-- even numbers that are equal or greater than 0
listOfEven = [2*x | x <- [0..]]

-- TASK 4
-- Working with lists

-- TASK 5
-- Currying

-- Task 6
-- Parametric polymorphism

-- complete the function "map'" that
-- takes a function f: (a -> b), a list [a]
-- and returns a list where the function f
-- is applied to all elements
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- Task 7
-- Partial functions

-- complete the function "fibSafe" that
-- returns Nothing if it's called with
-- a negative number
safeFib :: Integer -> Maybe Integer
safeFib n
    | n < 0     = Nothing
    | otherwise = Just (fib n)

-- create the function "head" that takes
-- a list and returns the first element of
-- that list
-- if the list is empty, return Nothing
-- write an appropriate type signature

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- function that catches a maybe?
