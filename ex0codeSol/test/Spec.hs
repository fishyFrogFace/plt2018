import Test.Hspec
import Test.QuickCheck

import Lib

genPos :: Gen Int
genPos = choose (1, 10000)

fib21 = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

cart = [(4,3),(4,7),(4,9),(6,3),(6,7),(6,9),(8,3),(8,7),(8,9)]

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