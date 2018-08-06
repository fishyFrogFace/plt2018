module Lib
    ( drop'
    , Complex(..)
    ) where

-- TASK 1

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
 
data Complex = Complex Double Double deriving (Eq) 
 
instance Show Complex where 
    show (Complex r i) 
        | i >= 0 = show r ++ "+" ++ show i ++ "i" 
        | otherwise = show r ++ "-" ++ show (abs i) ++ "i" 

-- (+), (*), abs, signum, fromInteger, ((-)|negate) 
instance Num Complex where 
    (+) (Complex r1 i1) (Complex r2 i2) = Complex (r1+r2) (i1+i2) 
    (*) (Complex r1 i1) (Complex r2 i2) = Complex (r1*r2 - i1*i2) (r1*i2 + i1*r2) 
    abs (Complex r i) = Complex (sqrt (r**2+i**2)) 0 
    signum (Complex r i) = Complex (r/a) (i/a)
                                where
                                a = sqrt (r**2+i**2)
    fromInteger int = Complex (fromInteger int) 0 
    (-) (Complex r1 i1) (Complex r2 i2) = Complex (r1-r2) (i1-i2) 
 
-- create a type class 
-- create types that are instances of the type class, inheritance? 
 
--person => student => teacher, publications, languages, vehicles
 
-- Task 3
-- Bounded parametric polymorphism pt. 2

-- make a function that ties together with the typeclass
-- that is created
