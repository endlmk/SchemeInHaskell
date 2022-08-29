module SimpleParserSpec (spec) where

import Test.Hspec
import SimpleParser
-- import Prelude (String, Double)

spec :: Spec
spec = do
    describe "TestCase" $ do
        it "parses string" $ do
            parseExpression "test" `shouldBe` Right (Atom (Symbol "test"))
        it "parses number" $ do
            parseExpression "123" `shouldBe` Right (Atom (Number 123))

