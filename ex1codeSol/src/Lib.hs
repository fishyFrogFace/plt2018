{-# LANGUAGE ParallelListComp #-}

module Lib
    ( add
    , fib
    , listOfEven
    , zipped
    , cartesian
    , takeInt
    , take'
    , map'
    , safeFib
    , safeHead
    ) where

import Prelude hiding (map, take)

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
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- TASK 3
-- List comprehensions

-- create a list "listOfEven" which contains all 
-- even numbers that are equal or greater than 0
listOfEven = [2*x | x <- [0..]]

-- create a list of tuples, "zipped"
-- where each tuple contains the nth entry
-- in the lists [1..26] and ['a'..'z']
zipped = [(x,y) | x <- [1..26] | y <- ['a'..'z']]

-- create a list that contains the cartesian
-- product of the two vectors [4, 6, 8]
-- and [3, 7, 9]
cartesian = [(x,y) | x <- [4, 6, 8], y <- [3, 7, 9]]

-- TASK 4
-- Working with lists

-- complete the function "takeInt" that
-- an integer n and a list of integers and
-- returns the first n elements of 
-- the list
takeInt :: Int -> [Int] -> [Int]
takeInt n lst
    | n < 0    = []
    | otherwise = takes n lst

takes 0 _      = []
takes _ []     = []
takes _ [x]    = [x]
takes n (x:xs) = x : takes (n-1) xs
 
-- TASK 5
-- Currying

-- Task 6
-- Parametric polymorphism

-- rewrite the function "takeInt" so that it
-- accepts a list of any type
-- hint: you probably don't have to change much

take' :: Int -> [a] -> [a]
take' n lst
    | n <= 0    = []
    | otherwise = takes n lst

-- complete the function "map'" that
-- takes a function f: (a -> b), a list [a]
-- and returns a list where the function f
-- is applied to all elements
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- Task 7
-- Partial functions

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

-- function that catches maybe + includes fmap?
