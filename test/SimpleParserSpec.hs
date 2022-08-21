module SimpleParserSpec (spec) where

import Test.Hspec
import SimpleParser
-- import Prelude (String, Double)

spec :: Spec
spec = do
    describe "TestCase" $ do
        it "parses empty string" $ do
            parse "" `shouldBe` Atom (Symbol "")
            -- (1 + 2) `shouldBe` 3
