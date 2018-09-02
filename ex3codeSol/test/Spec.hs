import Test.Hspec
import Test.QuickCheck

import Lib
import Prelude hiding (drop, takeWhile, dropWhile, break)
import qualified Prelude as P

recList = [1..5] ++ recList

main :: IO ()
main = hspec $ do
    describe "drop" $ do
      it "drops correct number of elements" $ do
        drop 2 "abc" `shouldBe` "c"
      it "returns empty list as appropriate" $ do
        drop 5 [] `shouldBe` ([] :: [Int])
      it "does nothing on index == 0" $ do
        drop 0 [1..10] `shouldBe` [1..10]
      it "does nothing on negative index" $ do
        drop (-1) [1..10] `shouldBe` [1..10]

    describe "takeWhile" $ do
      it "correctly takes positive elements" $ do
        takeWhile (>0) [3,2 .. -3] `shouldBe` [3,2,1]
      it "correctly handles empty-list" $ do
        takeWhile (==0) [] `shouldBe` ([] :: [Int])

    describe "dropWhile" $ do
      it "correctly drops positive elements" $ do
        dropWhile (>0) [3,2 .. -3] `shouldBe` [0,-1 .. -3]
      it "correctly handles empty-list" $ do
        dropWhile (==0) [] `shouldBe` ([] :: [Int])

    describe "break" $ do
      it "correctly breaks on zero" $ do
        break (==0) [-2..2] `shouldBe` ([-2,-1], [0..2])
      it "correctly handles empty-list" $ do
        break (==0) [] `shouldBe` ([] :: [Int], [] :: [Int])

    describe "splitOn" $ do
        it "splits a list of integers on a given integer" $ do
            P.take 3 (splitOn 1 recList) `shouldBe` replicate 3 [2,3,4,5]
        it "returns an empty list for empty list" $ do
            splitOn 'a' [] `shouldBe` ([] :: [String])
        it "removes excess elements" $ do
            splitOn ';' ";;;;this;;is;;;;;sparta;;;" `shouldBe` ["this", "is", "sparta"]
