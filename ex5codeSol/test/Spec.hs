import Test.Hspec
import Test.QuickCheck

import Lib

genNegList :: Gen [Int]
genNegList = sublistOf [-100, -1]

genList :: Gen [Int]
genList = listOf1 $ elements [3..20]

genNeg :: Gen Int
genNeg = choose (-25, -1)

fib21 = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

main :: IO ()
main = hspec $ do
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
