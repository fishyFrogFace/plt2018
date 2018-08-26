module Lib
    ( f0
    , f1
    , f2
    , take
    , map
    , iterate
    , filterPos
    , filterPosMany
    ) where

import Prelude hiding (map, take, iterate, sqrt)

-- TASK 1
-- Parametric polymorphism

-- Below are three type signatures. Can you implement them? We
-- say a function or implementation /inhabits/ it's type
-- (signature). How many other inhabitants do these types
-- have? What if we fixed a = Int, does that change your
-- previous answer?
f0 :: a -> a
f0 x = x

f1 :: a -> b -> a
f1 a _ = a

f2 :: a -> b -> b
f2 _ b = b

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

ourInfLst :: Double -> Double -> [Double]
ourInfLst x guess = iterate (newtons x) guess

doubleIsInt :: Double -> Bool
doubleIsInt x = fromInteger (round x) == x

approx :: Double -> [Double] -> Double
approx diff (x:y:xs)
    | abs (x-y) <= diff = y
    | otherwise         = approx diff (y:xs)

isPerfSq :: Double -> Bool
isPerfSq x = doubleIsInt . approx 0.00000001 $ ourInfLst x (x/2)

accuracy :: Int -> Bool
accuracy x = take x generated == take x [x^2 | x <- [1..]]
                where
             zpd       = zip [1..] (map isPerfSq [1..])
             f (x,y)   = y == True
             generated = fst . unzip $ filter f zpd
