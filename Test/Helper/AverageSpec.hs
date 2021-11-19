module Test.Helper.AverageSpec where

import Application.Helper.Average (average)
import IHP.FrameworkConfig
import IHP.HaskellSupport
import IHP.Prelude
import Test.Hspec

tests :: Spec
tests = describe "Average" $ do
  it "calculates average" $ do
    average [1.0 .. 5.0] `shouldBe` Just 3
    average [1 .. 5] `shouldBe` Just 3
    average [1.0, 11.0] `shouldBe` Just 6
    average [1.0, 12.0] `shouldBe` Just 6.5
    average [1.5, 2.5] `shouldBe` Just 2
    average [1.0] `shouldBe` Just 1

  it "does not calculate average of bad inputs" $ do
    average [] `shouldBe` Nothing
    average Nothing `shouldBe` Nothing
