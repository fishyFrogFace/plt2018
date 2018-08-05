module Lib
    ( fun
    , Complex(..)
    ) where

fun = undefined

-- TASK 1
-- Types, type classes and ad-hoc polymorphism

-- create a number type (complex too complex? natural?)
-- create a show instance
-- create a num instance
-- (+), (*), abs, signum, fromInteger, (negate | (-))

data Complex = Complex Double Double

instance Show Complex where
    show (Complex r i)
        | i >= 0 = show r ++ "+" ++ show i ++ "i"
        | otherwise = show r ++ "-" ++ show (abs i) ++ "i"
        
instance Num Complex where
    (+) (Complex r1 i1) (Complex r2 i2) = Complex (r1+r2) (i1+i2)
    (*) (Complex r1 i1) (Complex r2 i2) = Complex (r1*r2 - i1*i2) (r1*i2 + i1*r2)
    abs (Complex r i) = Complex (sqrt (r**2+i**2)) 0
    signum (Complex r i) = undefined --z/|z|, simplify?
    fromInteger int = Complex (fromInteger int) 0
    (-) (Complex r1 i1) (Complex r2 i2) = Complex (r1-r2) (i1-i2)

-- create a type class
-- create types that are instances of the type class
