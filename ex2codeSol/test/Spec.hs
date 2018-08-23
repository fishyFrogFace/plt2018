import Test.Hspec
import Test.QuickCheck

import Lib

genNeg :: Gen Int
genNeg = choose (-25, -1)

negList = [-10..(-1)]
mixList = [-10, -4, 347, -9, 0, 0, 2, -2, 0, 1, -27]
manyCheck = [[], negList, mixList]

recList = [1..5] ++ recList

main :: IO ()
main = hspec $ do

    describe "id'" $ do
        it "returns a String for a String input" $ do
            id' "a string" `shouldBe` id "a string"
        it "returns a Bool for a Bool input" $ do
            id' True `shouldBe` id True
        it "returns a Double for a Double input" $ do
            id' (3.0 :: Double) `shouldBe` id (3.0 :: Double)

    describe "take'" $ do
        it "returns an empty list for negative integers" $ do
            property $ forAll genNeg $ \n -> take' n [1..10] == ([]:: [Int])
        it "returns an empty list for n = 0" $ do
            take' 0 ['a'..'z'] `shouldBe` ([] :: [Char])
        it "returns an empty list for []" $ do
            take' 5 [] `shouldBe` ([] :: [Int])
        it "returns the n first elements of a list" $ do
            take' 5 ['a'..'z'] `shouldBe` ['a'..'e']
        it "returns the whole list if it's shorter than n" $ do
            take' 10 [1..5] `shouldBe` [1..5]

    describe "map'" $ do
        it "returns an empty list for f []" $ do
            map' (+1) [] `shouldBe` []
        it "applies a function to all elements" $ do
            map' (+2) [1..10] `shouldBe` [3..12]
   
    describe "filterPos" $ do
        it "returns empty list for empty list" $ do
            filterPos [] `shouldBe` []
        it "returns empty list for list of negative integers" $ do
            filterPos negList `shouldBe` []
        it "filters out negative entries of a list" $ do
            filterPos mixList `shouldBe` [347, 0, 0, 2, 0, 1]

    describe "filterPosMany" $ do
        it "filters all lists in a list" $ do
            filterPosMany manyCheck `shouldBe` [[], [], [347, 0, 0, 2, 0, 1]]
    
    describe "splitOn" $ do
        it "splits a list of integers on a given integer" $ do
            take 3 (splitOn 1 recList) `shouldBe` replicate 3 [2,3,4,5]
        it "returns an empty list for empty list" $ do
            splitOn 'a' [] `shouldBe` ([] :: [String])
        it "removes excess elements" $ do
            splitOn ';' ";;;;this;;is;;;;;sparta;;;" `shouldBe` ["this", "is", "sparta"]
