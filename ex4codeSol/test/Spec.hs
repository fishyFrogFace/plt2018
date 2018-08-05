import Test.Hspec
import Test.QuickCheck

import Lib
import Prelude hiding (lex)

main :: IO ()
main = hspec $ do
    describe "not implemented" $ do
        it "is not implemented" $ do
            "unfinished" `shouldBe` "unfinished"
