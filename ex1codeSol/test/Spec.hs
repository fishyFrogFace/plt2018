import Test.Hspec
import Test.QuickCheck

import Lib

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
