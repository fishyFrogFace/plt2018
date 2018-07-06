{-# LANGUAGE ParallelListComp #-}

module Lib
    ( add
    , fib
    , listOfEven
    , zipped
    , cartesian
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
add = undefined

-- TASK 2
-- Recursion

-- finish the function "fib" that calculates the
-- nth fibonnaci number 
-- assuming that 0th = 0 and 1st = 1
fib :: Integer -> Integer
fib = undefined

-- TASK 3
-- List comprehensions

-- create a list "listOfEven" which contains all 
-- even numbers that are equal or greater than 0
listOfEven = [undefined]

-- create a list of tuples, "zipped"
-- where each tuple contains the nth entry
-- in the lists [1..26] and ['a'..'z']
zipped = [undefined]

-- create a list that contains the cartesian
-- product of the two vectors [4, 6, 8]
-- and [3, 7, 9]
cartesian = [undefined]

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
map' = undefined

-- Task 7
-- Partial functions

-- complete the function "fibSafe" that
-- returns Nothing if it's called with
-- a negative number
safeFib :: Integer -> Maybe Integer
safeFib = undefined

-- create the function "head" that takes
-- a list and returns the first element of
-- that list
-- if the list is empty, return Nothing
-- write an appropriate type signature

-- safeHead :: undefined
safeHead = undefined

-- function that catches a maybe?
