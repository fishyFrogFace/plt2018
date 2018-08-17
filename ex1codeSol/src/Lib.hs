{-# LANGUAGE ParallelListComp #-}

module Lib
    ( add
    , isVowel
    , Status(..)
    , amountOf
    , fib
    , ending
    , takeInt
    , fizzbuzz
    , printFizz
    , listOfEven
    , zipped
    , cartesian
    ) where

import Control.Monad (mapM_)

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

data Status = One | Two | Three | None deriving (Show, Eq)

-- complete the function "subjects" which takes
-- a name and a list of elements
-- and pattern matches on the amount of elements
-- in the list
-- it returns a tuple of a Status (above)
-- and the name
amountOf :: String -> [a] -> (Status, String)
amountOf name []      = (None, name)
amountOf name [_]     = (One, name)
amountOf name [_,_]   = (Two, name)
amountOf name [_,_,_] = (Three, name)

-- TASK 2
-- Recursion

-- finish the function "fib" that calculates the
-- nth fibonacci number 
-- assuming that 0th = 0 and 1st = 1
-- do not optimize it
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- TASK 3
-- Working with lists

-- complete the function "ing" that takes a list of
-- strings and returns them with the ending -ing
-- if the string is empty, remove it from the list
ending :: [String] -> [String]
ending []     = []
ending (x:xs)
    | x == []   = ending xs
    | otherwise = (x ++ "ing") : ending xs

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

-- implement "fizzbuzz" as described in exercise 1
fizzbuzz :: [String]
fizzbuzz = fizz [1..100]
            where
           fizz [] = []
           fizz (x:xs)
                | x `mod` 15 == 0 = "FizzBuzz" : fizz xs
                | x `mod` 3 == 0  = "Fizz" : fizz xs
                | x `mod` 5 == 0  = "Buzz" : fizz xs
                | otherwise       = show x : fizz xs

printFizz :: IO ()
printFizz = mapM_ putStrLn fizzbuzz

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
