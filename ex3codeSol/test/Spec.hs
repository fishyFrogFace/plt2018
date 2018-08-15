import Test.Hspec
import Test.QuickCheck

import Lib
import Prelude hiding (lex)

genList :: Gen [Char]
genList = sublistOf $ '.':[':'..'~']

main :: IO ()
main = hspec $ do
    describe "lex" $ do
        it "splits a string on space" $ do
            lex "  we need    bees  " `shouldBe` ["we", "need", "bees"]

    describe "tokenize" $ do
        it "tokenizes operators" $ do
            tokenize ["*", "+", "-", "/"] `shouldBe` [TokOp Mult, TokOp Plus, TokOp Minus, TokOp Div]
        it "tokenizes integers" $ do
            tokenize ["1", "23", "3754924"] `shouldBe` [TokInt 1, TokInt 23, TokInt 3754924]
        it "tokenizes duplication operator" $ do
            tokenize ["#"] `shouldBe` [TokOp Dupl]
        it "tokenizes flip operator" $ do
            tokenize ["--"] `shouldBe` [TokOp Flip]
        it "recognizes erroneous strings" $ do
            property $ forAll genList $ \n -> tokenize [n] == [TokErr]
        it "returns empty list for empty input" $ do
            tokenize [] `shouldBe` ([] :: [Token])

    describe "interpret" $ do
        it "interprets a list of Tokens and returns the result" $ do
            interpret [TokInt 230, TokInt 5, TokOp Plus, TokInt 10, TokOp Minus] `shouldBe` [TokInt 225]
        it "returns empty list for empty input" $ do
            interpret [] `shouldBe` ([] :: [Token])
        it "flips the sign and duplicates elements" $ do
            interpret [TokInt 5, TokOp Dupl, TokOp Flip, TokOp Plus] `shouldBe` [TokInt 0]
        it "returns error token for erroneous input" $ do
            interpret [TokErr] `shouldBe` [TokErr]
