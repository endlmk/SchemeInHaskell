{-# LANGUAGE OverloadedStrings #-}

module SimpleParserSpec (spec) where

import SimpleParser
import Test.Hspec

-- import Prelude (String, Double)

spec :: Spec
spec = do
  describe "TestCase" $ do
    it "parses string" $ do
      parseExpression "test" `shouldBe` Right (Atom "test")
    it "parses number" $ do
      parseExpression "123" `shouldBe` Right (Number 123)
