module Test.Helper.Co2CalculationSpec where

import Application.Helper.Co2Calculation (calcAverageCo2Value, calcPossibleCo2PerYear)
import IHP.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck

data SourceSpec = SourceSpec
  { gCo2e :: Double,
    per :: Double
  }

data Co2ProducerSpec = Co2ProducerSpec
  { singleConsumptionAverage :: Double,
    timesPerYearAverage :: Double
  }

tests :: Spec
tests = describe "Co2 calculation" $ do
  it "calculates average co2 value" $ do
    calcAverageCo2Value [SourceSpec 3.0 1.0, SourceSpec 5.0 1.0] `shouldBe` Just 4.0
    calcAverageCo2Value [SourceSpec 4.0 2.0, SourceSpec 2.0 1.0] `shouldBe` Just 2.0

  it "calculates possible co2 per year" $ do
    calcPossibleCo2PerYear (Co2ProducerSpec 8.0 20.0) 4.0 `shouldBe` 640.0
    calcPossibleCo2PerYear (Co2ProducerSpec 100.0 100.0) 1000.0 `shouldBe` 10000000.0
    calcPossibleCo2PerYear (Co2ProducerSpec 10.0 10.0) (-5.0) `shouldBe` (-500.0)
