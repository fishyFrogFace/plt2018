{-# LANGUAGE ParallelListComp #-}

module Lib
    ( id'
    , take'
    , map'
    , filterPos
    , filterPosMany
    , splitOn
    , safeFib
    , safeHead
    , showHead
    , fibOfHead
    ) where

import Prelude hiding (map, take, id)

-- lazy/streams
bot = bot

const1 x = 1
 
-- TASK 1
-- Parametric polymorphism

-- complete the function "id'" that takes
-- any type and returns output of the same type
-- hint: there's only one possible function
-- that can do this
id' :: a -> a
id' x = x

-- rewrite the function "takeInt" so that it
-- accepts a list of any type
-- hint: you probably don't have to change much

take' :: Int -> [a] -> [a]
take' n lst
    | n <= 0    = []
    | otherwise = takes n lst

takes 0 _      = []
takes _ []     = []
takes _ [x]    = [x]
takes n (x:xs) = x : takes (n-1) xs

-- TASK 2
-- Higher order functions

-- complete the function "map'" that
-- takes a function f: (a -> b), a list [a]
-- and returns a list where the function f
-- is applied to all elements
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- TASK 3
-- Currying

-- complete the function filterPos
-- that takes a list and returns 
-- a filtered list containing only positive
-- integers
-- use currying to achieve this
filterPos :: [Int] -> [Int]
filterPos lst = filter (>=0) lst

-- complete the function filterPosMany
-- that takes a list of lists and returns
-- a list of lists with only positive
-- integers
-- hint: use filterPos and map'
filterPosMany :: [[Int]] -> [[Int]]
filterPosMany lst = map' filterPos lst

-- TASK 4
-- Bounded parametric polymorphism

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch lst = let strip = dropWhile (==ch) lst
                 in case strip of
                    []     -> []
                    (x:xs) -> n : (splitOn ch b)
                                where
                              (n, b) = break (==ch) strip

-- TASK 5
-- Laziness and streams

newtons :: Double -> Double -> Double
newtons x guess = guess - (guess^2 - x)/(2*guess)

approx :: Double -> [Double] -> Double
approx diff (x:y:xs)
    | abs (x-y) <= diff = y
    | otherwise         = approx diff (y:xs)

doubleIsInt :: Double -> Bool
doubleIsInt x = fromInteger (round x) == x

isSqrNum :: Double -> Double -> Bool
isSqrNum x guess = doubleIsInt . approx 0.0000001 $ ourInfLst x guess

ourInfLst :: Double -> Double -> [Double]
ourInfLst x guess = iterate (newtons x) guess

-- TASK 4
-- Partial functions
-- TODO: move to ex5?

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
