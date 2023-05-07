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
