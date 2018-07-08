{-# LANGUAGE ParallelListComp #-}

module Lib
    ( add
    , isVowel
    , fib
    , listOfEven
    , zipped
    , cartesian
    , takeInt
    , id'
    , take'
    , map'
    , filterPos
    , filterPosMany
    , safeFib
    , safeHead
    , showHead
    , fibOfHead
    ) where

import Prelude hiding (map, take, id)

-- TASK 1
-- Simple functions

-- finish the function "add" that takes two integers
-- and returns the sum of them
add :: Int -> Int -> Int
add = undefined

-- complete the function "isVowel" which
-- takes a character and returns True
-- if it's a vowel (English language), False otherwise
-- hint: a string is a list
-- hint2: use `elem` from Prelude
isVowel :: Char -> Bool
isVowel = undefined

-- TASK 2
-- Recursion

-- finish the function "fib" that calculates the
-- nth fibonacci number 
-- assuming that 0th = 0 and 1st = 1
fib :: Int -> Int
fib = undefined

-- TASK 3
-- List comprehensions

-- create a list "listOfEven" which contains all 
-- even numbers that are equal or greater than 0
-- use a list comprehension
listOfEven = [undefined]

-- create a list of tuples, "zipped"
-- where each tuple contains the nth entry
-- in the lists [1..26] and ['a'..'z']
-- hint: paralell list comprehension
zipped = [undefined]

-- create a list that contains the cartesian
-- product of the two vectors [4, 6, 8]
-- and [3, 7, 9]
-- use a list comprehension
cartesian = [undefined]

-- TASK 4
-- Working with lists

-- complete the function "takeInt" that
-- an integer n and a list of integers and
-- returns the first n elements of 
-- the list
takeInt :: Int -> [Int] -> [Int]
takeInt = undefined

-- Task 5
-- Parametric polymorphism

-- complete the function "id'" that takes
-- any type and returns output of the same type
-- hint: there's only one possible function
-- that can do this
id' :: a -> a
id' = undefined

-- rewrite the function "takeInt" so that it
-- accepts a list of any type
-- hint: you probably don't have to change much

take' = undefined

-- complete the function "map'" that
-- takes a function f: (a -> b), a list [a]
-- and returns a list where the function f
-- is applied to all elements
map' :: (a -> b) -> [a] -> [b]
map' = undefined

--Task 6
-- Currying

-- complete the function filterPos
-- that takes a list and returns 
-- a filtered list containing only positive
-- integers
-- use currying to achieve this
filterPos :: [Int] -> [Int]
filterPos = undefined

-- complete the function filterPosMany
-- that takes a list of lists and returns
-- a list of lists with only positive
-- integers
-- hint: use filterPos and map'
filterPosMany :: [[Int]] -> [[Int]]
filterPosMany = undefined

-- Task 7
-- Partial functions

-- complete the function "safeFib" that
-- returns Nothing if it's called with
-- a negative number
safeFib :: Int -> Maybe Int
safeFib = undefined

-- create the function "safeHead" that takes
-- a list and returns the first element of
-- that list
-- if the list is empty, return Nothing
-- write an appropriate type signature

--safeHead :: undefined
safeHead = undefined

-- complete the function "showHead" that
-- takes a list and returns the String
-- "The first element is <x>" if the list
-- is not empty and the String "The list is
-- empty" if there is no first element
-- use "safeHead" to do this
showHead :: Show a => [a] -> String
showHead = undefined

-- OPTIONAL EXERCISE
-- complete the function "fibOfHead"
-- that takes a list of integers, takes
-- the first element of the list, n
-- and returns the nth fibonacci number
-- you will need to combine "safeHead"
-- and safeFib
-- hint: (>>=) :: Monad m => m a -> (a -> m b) -> m b 
fibOfHead :: [Int] -> Maybe Int
fibOfHead = undefined
