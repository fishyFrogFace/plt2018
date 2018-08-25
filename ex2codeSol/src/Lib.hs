module Lib
    ( id
    , fst
    , snd
    , take
    , map
    , iterate
    , filterPos
    , filterPosMany
    ) where

import Prelude hiding (map, take, id, iterate, fst, snd)

-- TASK 1
-- Parametric polymorphism

-- Below are three type signatures. Can you implement them? We
-- say a function or implementation /inhabits/ it's type
-- (signature). How many other inhabitants do these types
-- have? What if we fixed a = Int, does that change your
-- previous answer?
id :: a -> a
id = undefined

fst :: a -> b -> a
fst = undefined 

snd :: a -> b -> b
snd = undefined

-- rewrite the function "takeInt" so that it
-- accepts a list of any type
-- if you used the built in function "take" on the
-- last assignment, write your own implementation of it
-- hint: you probably don't have to change much

take :: Int -> [a] -> [a]
take n lst
    | n <= 0    = []
    | otherwise = takes n lst

takes 0 _      = []
takes _ []     = []
takes _ [x]    = [x]
takes n (x:xs) = x : takes (n-1) xs

-- TASK 2
-- Higher order functions

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- TASK 3
-- Currying and partial application

-- complete the function filterPos
-- that takes a list and returns 
-- a filtered list containing only positive
-- integers
-- use partial application to achieve this
filterPos :: [Int] -> [Int]
filterPos lst = filter (>=0) lst

-- complete the function filterPosMany
-- that takes a list of lists and returns
-- a list of lists with only positive
-- integers
-- hint: use filterPos and map
filterPosMany :: [[Int]] -> [[Int]]
filterPosMany lst = map filterPos lst

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f c b a = f a b c

-- TASK 4
-- Infinite lists

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
