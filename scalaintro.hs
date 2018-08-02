--ghc 8.2

import Data.Traversable
import Prelude hiding (enumFromTo)
-- 1a
helloworld = putStrLn "hello world!"

main = do
    let x = [1, 2, 3]
    print $ "There are " ++ show (length x) ++ " elements"
    for x print

-- two ways to create the list in 1b
makeArray :: [Int]
makeArray = mke [] 50
                where
                mke lst 0 = lst
                mke lst n = mke (n:lst) (n-1)

range = [1..50]

-- append using map in haskell is not possible in the same way as scala because of immutability
--  // 1b
--  var generated: Array[Int] = Array()
--  for (i <- 1 to 50) generated :+= i
--  // 1c
--  (51 to 100).map((x: Int) => generated :+= x

append :: [a] -> [a] -> [a]
append [] lst     = lst
append (x:xs) lst = x : append xs lst

-- two ways to sum a list (1d and 1e)
sum1 = foldl (+) 0 [1..10]

sum2 :: Num a => [a] -> a
sum2 []     = 0
sum2 (x:xs) = x + sum2 xs

-- 1f fibonacci is in exercise 1
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- task 2 is about threads, which we don't talk about on in haskell

-- haskell laziness difference between scala and haskell?
-- haskell is lazy by default and does not evaluate expressions unless a result is needed, for example pattern matching
-- or printing. adding computation will not lead to evaluation

-- this is for clarification only and does not need to be part of the exercise
-- haskell laziness will be addressed in exercise 2
-- adapted from https://hackhands.com/modular-code-lazy-evaluation-haskell/:
-- [1..30] is syntactic sugar for "enumFromTo 1 30"
-- this is a simplified version of the function
enumFromTo :: Integer -> Integer -> [Integer]
enumFromTo n m
    | n == m    = []
    | otherwise = n : enumFromTo (n+1) m

lazyEx = do
        let x = [1..30]    --not evaluated, stored as "enumFromTo 1 30"
        let y = map fib x  --adding computations does not lead to evaluation, neither x nor y is evaluated here
        print y            --x and y is evaluated: "1 : map fib (enumFromTo 2 30)" (TODO: double check evaluation order, clarify)

-- see also https://wiki.haskell.org/Non-strict_semantics
