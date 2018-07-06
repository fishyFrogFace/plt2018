import Test.Hspec
import Test.QuickCheck

import Lib

fib21 = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

genPos :: Gen Int
genPos = choose (1, 10000)

genNeg :: Gen Int
genNeg = choose (-25, -1)

genList :: Gen [Int]
genList = listOf1 $ choose (-100, 100)

cart = [(4,3),(4,7),(4,9),(6,3),(6,7),(6,9),(8,3),(8,7),(8,9)]
negList = [-10..(-1)]
mixList = [-10, -4, 347, -9, 0, 0, 2, -2, 0, 1, -27]
manyCheck = [[], negList, mixList]

vowels = "aeiouAEIOU"
nonVowels = "0123456789:;<=>?@BCDFGHJKLMNPQRSTVWXYZ[\\]^_`bcdfghjklmnpqrstvwxyz"

main :: IO ()
main = hspec $ do

    describe "add" $ do
        it "can compute 2 + 2 = 4" $ do
            add 2 2 `shouldBe` 4

        it "has zero as a left identity" $ do
            property $ \n -> add 0 n == n

        it "has zero as a right identity" $ do
            property $ \n -> add n 0 == n

        it "is commutative" $ do
            property $ \m n -> add m n == add n m

        it "is associative" $ do
            property $ \m n o -> add m (add n o) == add (add m n) o

    describe "isVowel" $ do
        it "returns True for all vowels" $ do
            map isVowel vowels `shouldBe` replicate (length vowels) True
        it "returns False for other characters" $ do
            map isVowel nonVowels `shouldBe` replicate (length nonVowels) False

    describe "fib" $ do
        it "can compute the first 21 fibonnaci numbers" $ do
            map fib [0..20] `shouldBe` fib21
    
    describe "listOfEven" $ do
        it "is an infinite list of even numbers" $ do
            property $ forAll genPos $ \n -> listOfEven !! n == toInteger (2*n)

    describe "zipped" $ do
        it "equals the zipped list of [1..26] and ['a'..'z']" $ do
            zipped `shouldBe` zip [1..26] ['a'..'z']

    describe "cartesian" $ do
        it "equals the cartesian product of [4,6,8] and [3,7,9]" $ do
            cartesian `shouldBe` cart

    describe "takeInt" $ do
        it "returns an empty list for negative integers" $ do
            property $ forAll genNeg $ \n -> takeInt n [1..10] == []
        it "returns an empty list for n = 0" $ do
            takeInt 0 [1..10] `shouldBe` []
        it "returns an empty list for []" $ do
            takeInt 5 [] `shouldBe` ([] :: [Int])
        it "returns the n first elements of a list" $ do
            takeInt 5 [1..10] `shouldBe` [1..5]
        it "returns the whole list if it's shorter than n" $ do
            takeInt 10 [1..5] `shouldBe` [1..5]

    describe "id'" $ do
        it "returns a String for a String input" $ do
            id' "a string" `shouldBe` id "a string"
        it "returns a Bool for a Bool input" $ do
            id' True `shouldBe` id True
        it "returns a Double for a Double input" $ do
            id' (3.0 :: Double) `shouldBe` id (3.0 :: Double)

    describe "take'" $ do
        it "returns an empty list for negative integers" $ do
            property $ forAll genNeg $ \n -> take' n [1..10] == ([] :: [Int])
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

    describe "safeFib" $ do
        it "returns Nothing for negative Integers" $ do
            property $ forAll genNeg $ \n -> safeFib n == Nothing
        it "returns Just nth for the nth fibonnacci number" $ do
            map safeFib [0..20] `shouldBe` map (\n -> Just n) fib21
    
    describe "safeHead" $ do
        it "returns Nothing for empty list" $ do
            safeHead [] `shouldBe` (Nothing :: Maybe Int)
        it "returns Just x for x:tail" $ do
            property $ forAll genList $ \n -> safeHead n == Just (head n)
