module Tree
    (Tree(..)
    , treeSum
    , treeConcat
    , treeMaximum) where

-- TASK 3.2
-- Binary Trees

data Tree a = Branch (Tree a) a (Tree a) | Leaf a
  deriving (Eq, Show)

-- Either define each of the following specifically for our Tree
-- data-structure, or define a Foldable instance (below) and get them
-- for free. The Foldable instance might prove tricky to define, so
-- defining the specific functions first may be easier!
treeSum :: (Num a) => Tree a -> a
treeSum (Leaf a) = a
treeSum (Branch left x right) = treeSum left + x + treeSum right

treeConcat :: Tree String -> String
treeConcat (Leaf s) = s
treeConcat (Branch left x right) = treeConcat left ++ x ++ treeConcat right

-- Implement treeMaximum and treeMinimum and give them appropriate
-- type signatures. Do you need to return a Maybe? Why / why not?
treeMaximum :: (Ord a) => Tree a -> a
treeMaximum (Leaf a) = a
treeMaximum (Branch left x right) = max (max (treeMaximum left) x) (treeMaximum right)

-- Write a Foldable instance for Tree.
instance Foldable Tree where
  foldr op acc (Leaf a) = a `op` acc
  foldr op acc (Branch left x right) =
    foldr op (x `op` foldr op acc right) left
