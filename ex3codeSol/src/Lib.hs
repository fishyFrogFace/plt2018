module Lib
    ( splitOn
    , drop
    , takeWhile
    , dropWhile
    , break
    , Complex(..)
    ) where

import Prelude hiding (takeWhile, dropWhile, break, drop)

-- TASK 1
-- Bounded parametric polymorphism

-- Implement "drop" (the "opposite" of take, seen before)
-- but let GHC infer it's type signature. Check out what GHC
-- infers using ":t" inside GHCi. Is there a problem?
-- drop (Eq t, Num t) => t -> [a] -> [a]
-- answer: Num t is too general for index; should be Int (or Integer)
drop :: Int -> [a] -> [a]
drop _ []     = []
drop n l@(x:xs)
    | n > 0     = drop (n-1) xs
    | otherwise = l

-- Implement the following functions which respectively
-- return the elements at the beginning of a list satisfying
-- the given predicate; or drops them, returning the
-- remaining elements of the list.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p l@(x:xs)
  | p x       = dropWhile p xs
  | otherwise = l

-- Implement "break" which splits a list into two pieces,
-- one consisting of elements before a certain predicate is
-- satisfied, and then the remaining elements (including the
-- element satisfying the predicate).
break :: (a -> Bool) -> [a] -> ([a], [a])
break p l = (start, end)
  where start = takeWhile (not. p) l
        end = drop (length start) l

-- Implement "splitOn" which splits a list into non-empty
-- segments separated by a given character.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch lst = let strip = dropWhile (==ch) lst
                 in case strip of
                    []     -> []
                    (x:xs) -> n : (splitOn ch b)
                                where
                                  (n, b) = break (==ch) strip


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
