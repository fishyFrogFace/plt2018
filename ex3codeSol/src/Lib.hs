{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
-- Num Complex
 
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

-- TASK 3
-- Making your own type classes

type Position = (Double, Double)

class Pos a where
    pos :: a -> Position

class (Pos a) => Move a where
    relocate :: a -> Position -> a
    belongs :: a -> Position

data Campus = Kalvskinnet
            | Gløshaugen
            | Tyholt
            | Moholt
            | Dragvoll
            deriving (Show, Eq)

instance Pos Campus where
    pos Kalvskinnet = (63.429, 10.388)
    pos Gløshaugen  = (63.416, 10.403)
    pos Tyholt      = (63.423, 10.435)
    pos Moholt      = (63.413, 10.434)
    pos Dragvoll    = (63.409, 10.471)

data Car = Car { brand :: String
               , regnr :: String
               , isAt :: Position
               , key :: Key
               , parking :: Position} deriving (Show)

instance Eq Car where
    (==) car1 car2 = regnr car1 == regnr car2

instance Pos Car where
    pos = isAt

instance Move Car where
    relocate car loc = car { isAt = loc }
    belongs = parking

data Key = Key { keynr :: Int
               , located :: Position
               , cabinet :: Position } deriving (Show)

instance Pos Key where
    pos = located

instance Move Key where
    relocate key loc = key { located = loc }
    belongs = cabinet

free :: Move a => a -> Bool
free object = pos object == belongs object

carAvailable :: Car -> Bool
carAvailable car = free car || (free $ key car)

distBetween :: Pos a => a -> a -> Position
distBetween loc object = (abs $ loc1-obj1 , abs $ loc2-obj2)
                            where
                            (loc1, loc2) = pos loc
                            (obj1, obj2) = pos object
