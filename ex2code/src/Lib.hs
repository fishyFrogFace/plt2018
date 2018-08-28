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
f0 = undefined

f1 :: a -> b -> a
f1 = undefined

f2 :: a -> b -> b
f2 = undefined

-- rewrite the function "takeInt" so that it
-- accepts a list of any type
-- if you used the built in function "take" on the
-- last assignment, write your own implementation of it
-- be sure to include a type signature
-- hint: you probably don't have to change much

take = undefined

-- TASK 2
-- Higher order functions

map :: (a -> b) -> [a] -> [b]
map = undefined

iterate :: (a -> a) -> a -> [a]
iterate = undefined

-- TASK 3
-- Currying and partial application

-- complete the function filterPos
-- that takes a list and returns 
-- a filtered list containing only positive
-- integers (including zero)
-- use partial application to achieve this
filterPos :: [Int] -> [Int]
filterPos = undefined

-- complete the function filterPosMany
-- that takes a list of lists and returns
-- a list of lists with only positive
-- integers (including zero)
-- hint: use filterPos and map
filterPosMany :: [[Int]] -> [[Int]]
filterPosMany = undefined

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 = undefined

-- TASK 4
-- Infinite lists

isPerfSq :: Double -> Bool
isPerfSq = undefined

--uncomment when isPerfSqr is defined
--accuracy :: Int -> Bool
--accuracy x = take x generated == take x [x^2 | x <- [1..]]
--                where
--             zpd       = zip [1..] (map isPerfSq [1..])
--             f (x,y)   = y == True
--             generated = fst . unzip $ filter f zpd
