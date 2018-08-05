module Lib
    ( drop'
    , fun
    ) where

fun = undefined

--bounded polymorphism (Eq a => a -> Bool), currying, recursion, HOF, type inference, lists/streams/laziness

-- TASK 1
-- Bounded parametric polymorphism

-- make a function that ties together with the typeclass
-- and types that are created

-- usage of ad-hoc polymorphism

-- TASK 2
-- Type inference

--what happens if we let the compiler decide the type signature?
--drop' (Eq t, Num t) => t -> [a] -> [a]
--why is this bad?
drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' n (x:xs)
    | n > 0     = drop' (n-1) xs
    | otherwise = (x:xs)

-- TASK 4
-- Pattern matching

-- explore pattern matching in functions? maybe more heavy on theory?

-- TASK 5
-- Infinite lists and laziness
