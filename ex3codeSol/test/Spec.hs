import Test.Hspec
import Test.QuickCheck

import Lib

recList = [1..5] ++ recList

main :: IO ()
main = hspec $ do
    describe "splitOn" $ do
        it "splits a list of integers on a given integer" $ do
            Prelude.take 3 (splitOn 1 recList) `shouldBe` replicate 3 [2,3,4,5]
        it "returns an empty list for empty list" $ do
            splitOn 'a' [] `shouldBe` ([] :: [String])
        it "removes excess elements" $ do
            splitOn ';' ";;;;this;;is;;;;;sparta;;;" `shouldBe` ["this", "is", "sparta"]
