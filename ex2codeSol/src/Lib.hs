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

-- TASK 3

type Position = (Double, Double)

class Pos a where
    position :: a -> Position

class (Pos a) => Move a where
    relocate :: a -> Position -> a
    belongs :: a -> Position
    free :: a -> Bool

data Campus = Kalvskinnet
            | Gløshaugen
            | Tyholt
            | Moholt
            | Dragvoll
            deriving (Show, Eq)

instance Pos Campus where
    position Kalvskinnet = (63.429, 10.388)
    position Gløshaugen  = (63.416, 10.403)
    position Tyholt      = (63.423, 10.435)
    position Moholt      = (63.413, 10.434)
    position Dragvoll    = (63.409, 10.471)

data Car = Car { brand :: String
               , regnr :: String
               , isAt :: Position
               , parking :: Position} deriving (Show)

instance Eq Car where
    (==) car1 car2 = regnr car1 == regnr car2

instance Pos Car where
    position = isAt

instance Move Car where
    relocate car loc = car { isAt = loc }
    belongs = parking
    free car = isAt car == parking car

data Person = Employee { fname :: String
                       , lname :: String
                       , located :: Position
                       , keys :: [Int]} deriving (Show)

-- Task 3
-- Ad-hoc polymorphism

-- make a function that ties together with the typeclass
-- that is created
