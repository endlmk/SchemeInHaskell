module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
    describe "TestCase" $ do
        it "somefunc" $ do
            (1 + 2) `shouldBe` 3
