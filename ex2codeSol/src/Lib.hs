module Lib
    ( drop'
    , Tree(..)
    ) where

-- TASK 1
-- Type inference

--what happens if we let the compiler decide the type signature?
--drop' (Eq t, Num t) => t -> [a] -> [a]
--why is this bad?
drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' n (x:xs)
    | n > 0     = drop' (n-1) xs
    | otherwise = (x:xs)

-- TASK 2
-- Types, type classes and ad-hoc polymorphism

-- create a number type (complex too complex? natural?)
-- create a show instance
-- create a num instance
-- (+), (*), abs, signum, fromInteger, (negate | (-))

-- create a type class
-- create types that are instances of the type class

data Tree a = undefined

-- TASK 3
-- Bounded parametric polymorphism

-- make a function that ties together with the typeclass
-- and types that are created

-- usage of ad-hoc polymorphism

-- TASK 4
-- Pattern matching

-- explore pattern matching in functions? maybe more heavy on theory?

-- TASK 5
-- Infinite lists and laziness
