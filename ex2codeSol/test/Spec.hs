import Test.Hspec
import Test.QuickCheck

import Lib
import List
import Prelude hiding (lex)

genList :: Gen [Char]
genList = sublistOf ['1'..'z']

recList = [1..5] ++ recList

main :: IO ()
main = hspec $ do

    describe "len" $ do
        it "accurately computes the length of a list" $ do
            property $ forAll genList $ \n -> len n == length n
        it "returns 0 for empty list" $ do
            len [] `shouldBe` 0
    
    describe "drop'" $ do
        it "drops n items of a list with more than n items" $ do
            drop' 5 [1..10] `shouldBe` [6..10]
        it "drops the whole list when less than n items" $ do
            drop' 47 ['a'..'z'] `shouldBe` []
        it "returns whole list when n is negative" $ do
            drop' (-3) "monkey" `shouldBe` "monkey"

    describe "append" $ do
        it "appends one list to another" $ do
            property $ forAll genList $ \n m -> append n m == n ++ m
        it "returns first list if second is empty" $ do
            append [1..10] [] `shouldBe` [1..10]
        it "returns second list if first list is empty" $ do
            append [] [1..10] `shouldBe` [1..10]
        it "returns empty list if both lists are empty" $ do
            append [] [] `shouldBe` ([] :: [Int])
    
    describe "member" $ do
        it "returns True if the element is in the list" $ do
            member 'a' "salad" `shouldBe` True
        it "retuns False if the element is not in the list" $ do
            member 'a' "SALAD" `shouldBe` False
        it "returns False if the list is empty" $ do
            member 5 [] `shouldBe` False

    describe "position" $ do
        it "returns the position if the element is in the list" $ do
            position 4 [0..10] `shouldBe` Just 4
        it "returns Nothing if the element is not in the list" $ do
            position 'a' ['b'..'z'] `shouldBe` (Nothing :: Maybe Int)
        it "returns Nothing for empty list" $ do
            position (Just 'a') [] `shouldBe` (Nothing :: Maybe Int)

    describe "splitOn" $ do
        it "splits a list of integers on a given integer" $ do
            take 3 (splitOn 1 recList) `shouldBe` replicate 3 [2,3,4,5]
        it "returns an empty list for empty list" $ do
            splitOn 'a' [] `shouldBe` ([] :: [String])
        it "removes excess elements" $ do
            splitOn ';' ";;;;this;;is;;;;;sparta;;;" `shouldBe` ["this", "is", "sparta"]

    describe "lex" $ do
        it "splits a string on space" $ do
            lex "  we need    bees  " `shouldBe` ["we", "need", "bees"]
