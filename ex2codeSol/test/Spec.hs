import Test.Hspec
import Test.QuickCheck

import Lib

fib21 = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

genNeg :: Gen Int
genNeg = choose (-25, -1)

genList :: Gen [Int]
genList = listOf1 $ elements [3..20]

genNegList :: Gen [Int]
genNegList = sublistOf [-100, -1]

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

    describe "safeFib" $ do
        it "returns Nothing for negative Integers" $ do
            property $ forAll genNeg $ \n -> safeFib n == Nothing
        it "returns Just nth for the nth fibonacci number" $ do
            map safeFib [0..20] `shouldBe` map (\n -> Just n) fib21
    
    describe "safeHead" $ do
        it "returns Nothing for empty list" $ do
            safeHead [] `shouldBe` (Nothing :: Maybe Int)
        it "returns Just x for x:tail" $ do
            property $ forAll genList $ \n -> safeHead n == Just (head n)

    describe "showHead" $ do
        it "returns \"The list is empty\" for empty list" $ do
            showHead ([] :: [Int]) `shouldBe` "The list is empty"
        it "returns \"The first element is <x>\" for x:tail" $ do
            property $ forAll genList $ \n -> showHead n == "The first element is " ++ show (head n)

    describe "fibOfHead (OPTIONAL EXERCISE)" $ do
        it "returns Nothing for negative Integers" $ do
            property $ forAll genNegList $ \n -> fibOfHead n == Nothing
        it "returns Just nth for the nth fibonacci number" $ do
            map fibOfHead [[x] | x <- [0..20]] `shouldBe` map (\n -> Just n) fib21 
        it "returns Nothing for empty list" $ do
            fibOfHead [] `shouldBe` (Nothing :: Maybe Int)
