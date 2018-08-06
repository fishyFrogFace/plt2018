{-# LANGUAGE ParallelListComp #-}

module Lib
    ( add
    , isVowel
    , fib
    , fizzbuzz
    , listOfEven
    , zipped
    , cartesian
    ) where

-- TASK 1
-- Simple functions

-- finish the function "add" that takes two integers
-- and returns the sum of them
add :: Int -> Int -> Int
add n m = n + m

-- complete the function "isVowel" which
-- takes a character and returns True
-- if it's a vowel (English language), False otherwise
-- hint: a string is a list
-- hint2: use `elem` from Prelude
isVowel :: Char -> Bool
isVowel chr
    | chr `elem` "aeiouAEIOU" = True
    | otherwise               = False

--list function without recursion

--tuple function

-- TASK 2
-- Recursion

-- create fizzbuzz, a list from 1 to 100
-- where every 3rd element is "Fizz", every
-- 5th element is "Buzz" and every 15th
-- element is "FizzBuzz"
-- hint: use the function "show" from Prelude
fizzbuzz :: [String]
fizzbuzz = fizz [1..100]
            where
           fizz [] = []
           fizz (x:xs)
                | x `mod` 15 == 0 = "FizzBuzz" : fizz xs
                | x `mod` 3 == 0  = "Fizz" : fizz xs
                | x `mod` 5 == 0  = "Buzz" : fizz xs
                | otherwise       = show x : fizz xs

-- finish the function "fib" that calculates the
-- nth fibonacci number 
-- assuming that 0th = 0 and 1st = 1
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- TASK 3
-- List comprehensions

-- create a list "listOfEven" which contains all 
-- even numbers that are equal or greater than 0
-- use a list comprehension
listOfEven :: [Integer]
listOfEven = [2*x | x <- [0..]]

-- create a list of tuples, "zipped"
-- where each tuple contains the nth entry
-- in the lists [1..26] and ['a'..'z']
-- hint: parallel list comprehension
zipped :: [(Int, Char)]
zipped = [(x,y) | x <- [1..26] | y <- ['a'..'z']]

-- create a list that contains the cartesian
-- product of the two vectors [4, 6, 8]
-- and [3, 7, 9]
-- use a list comprehension
cartesian :: [(Int, Int)]
cartesian = [(x,y) | x <- [4, 6, 8], y <- [3, 7, 9]]
