module List 
    ( len
    , drop'
    , append
    , member
    , position
    ) where

import Prelude hiding (length, drop, (++), elem)

len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

--take implemented in last exercise

--what happens if we let the compiler decide the type signature?
--drop' (Eq t, Num t) => t -> [a] -> [a]
--why is this bad?
drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' n (x:xs)
    | n > 0     = drop' (n-1) xs
    | otherwise = (x:xs)

append :: [a] -> [a] -> [a]
append [] lst = lst
append (x:xs) lst = x : append xs lst

--bounded parametric polymorphism
member :: Eq a => a -> [a] -> Bool
member _ [] = False
member el (x:xs)
    | el == x   = True
    | otherwise = member el xs

position :: Eq a => a -> [a] -> Maybe Int
position _ []  = Nothing
position n lst = pos 0 n lst
                    where
                 pos _ _ [] = Nothing
                 pos inc n (x:xs)
                    | x == n    = Just inc
                    | otherwise = pos (inc+1) n xs
