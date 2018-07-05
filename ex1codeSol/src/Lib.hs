module Lib
    ( add
    , fib
    ) where

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
