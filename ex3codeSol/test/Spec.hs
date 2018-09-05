import Test.Hspec
import Test.QuickCheck

import Lib

main :: IO ()
main = hspec $ do
    describe "drop" $ do
      it "drops correct number of elements" $ do
        drop 2 "abc" `shouldBe` "c"
