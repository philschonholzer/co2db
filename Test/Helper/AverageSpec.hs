module Test.Helper.AverageSpec where

import Application.Helper.Average (average)
import IHP.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck

tests :: Spec
tests = describe "Average" $ do
  it "calculates average" $ do
    average [1.0 .. 5.0] `shouldBe` Just 3
    average [1 .. 5] `shouldBe` Just 3
    average [1.0, 11.0] `shouldBe` Just 6
    average [1.0, 12.0] `shouldBe` Just 6.5
    average [1.5, 2.5] `shouldBe` Just 2
    average [1.0] `shouldBe` Just 1
    average [4.1] `shouldBe` Just 4.1
    average [0.0] `shouldBe` Just 0.0

  it "does not calculate average of bad inputs" $ do
    average [] `shouldBe` Nothing
    average Nothing `shouldBe` Nothing

  prop "largest number in the list is higher-equal than the average" $ \(x :: [Double]) ->
    average x
      `shouldSatisfy` \case
        Nothing -> null x
        Just result -> result <= maximum x

  prop "smallest number in the list is smaller-equal than the average" $ \(x :: [Double]) ->
    average x
      `shouldSatisfy` \case
        Nothing -> null x
        Just result -> result >= minimum x
