import Test.Hspec
import Test.QuickCheck

import Lib

fib21 = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

genPos :: Gen Int
genPos = choose (1, 10000)

genNeg :: Gen Integer
genNeg = choose (-25, -1)

genList :: Gen [Int]
genList = listOf1 $ choose (-100, 100)

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

    describe "fib" $ do
        it "can compute the first 21 fibonnaci numbers" $ do
            map fib [0..20] `shouldBe` fib21
    
    describe "listOfEven" $ do
        it "is an infinite list of even numbers" $ do
            property $ forAll genPos $ \n -> listOfEven !! n == toInteger (2*n)
    
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
