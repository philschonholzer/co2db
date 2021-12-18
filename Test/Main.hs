module Main where

import IHP.Prelude
import Test.Helper.AverageSpec
import Test.Helper.Co2CalculationSpec
import Test.Helper.ViewSpec
import Test.Hspec

main :: IO ()
main = hspec do
  Test.Helper.ViewSpec.tests
  Test.Helper.AverageSpec.tests
  Test.Helper.Co2CalculationSpec.tests
