{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Parser
import Test.Hspec

-- import Prelude (String, Double)

spec :: Spec
spec = do
  describe "TestCase" $ do
    it "parses string" $ do
      parseExpression "test" `shouldBe` Right (Atom "test")
    it "parses number" $ do
      parseExpression "123" `shouldBe` Right (Number 123)
    it "parses negative number" $ do
      parseExpression "-2187" `shouldBe` Right (Number (-2187))
    it "parses string" $ do
      parseExpression "\"Test\"" `shouldBe` Right (String "Test")
