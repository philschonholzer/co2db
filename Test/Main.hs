module Main where

import IHP.Prelude
import Test.Helper.AverageSpec
import Test.Helper.Co2CalculationSpec
import Test.Helper.ViewSpec
import Test.Hspec
import Test.Web.Controller.Co2ProducersSpec
import Test.Web.PathsSpec
import Test.Web.RoutesSpec

main :: IO ()
main = hspec do
  Test.Helper.ViewSpec.tests
  Test.Helper.AverageSpec.tests
  Test.Helper.Co2CalculationSpec.tests
  Test.Web.RoutesSpec.tests
  Test.Web.PathsSpec.tests
  Test.Web.Controller.Co2ProducersSpec.tests
