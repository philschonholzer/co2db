module Test.Helper.ViewSpec where

import Application.Helper.View (formatDecimal)
import IHP.FrameworkConfig
import IHP.HaskellSupport
import IHP.Prelude
import Test.Hspec

tests :: Spec
tests = do
  formatDecimalSpec

formatDecimalSpec :: Spec
formatDecimalSpec = describe "Format numbers" $ do
  it "formats 0 to percision of 2" $ do
    formatDecimal 0 `shouldBe` "0.00"

  it "formats numbers below 1 with percision 3" $ do
    formatDecimal 0.4 `shouldBe` "0.400"
    formatDecimal 0.04 `shouldBe` "0.040"
    formatDecimal 0.004 `shouldBe` "0.004"
    formatDecimal 0.0005 `shouldBe` "0.001"
    formatDecimal 0.0004 `shouldBe` "~ 0.000"
    formatDecimal 0.00004 `shouldBe` "~ 0.000"
    formatDecimal 0.000004 `shouldBe` "~ 0.000"

  it "formats numbers below 1 with percision of maximum of 3" $ do
    formatDecimal 0.400001 `shouldBe` "0.400"
    formatDecimal 0.040001 `shouldBe` "0.040"
    formatDecimal 0.004001 `shouldBe` "0.004"
    formatDecimal 0.000501 `shouldBe` "0.001"
    formatDecimal 0.000499 `shouldBe` "~ 0.000"
    formatDecimal 0.000401 `shouldBe` "~ 0.000"

  it "formats numbers from 1 inclusive to 10 exclusive with percision of 2" $ do
    formatDecimal 1.0 `shouldBe` "1.00"
    formatDecimal 1.1 `shouldBe` "1.10"
    formatDecimal 1.01 `shouldBe` "1.01"
    formatDecimal 1.001 `shouldBe` "1.00"
    formatDecimal 1.0001 `shouldBe` "1.00"
    formatDecimal 1.00001 `shouldBe` "1.00"
    formatDecimal 1.009 `shouldBe` "1.01"
    formatDecimal 1.009 `shouldBe` "1.01"
    formatDecimal 9.0 `shouldBe` "9.00"
    formatDecimal 9.9 `shouldBe` "9.90"
    formatDecimal 9.09 `shouldBe` "9.09"
    formatDecimal 9.009 `shouldBe` "9.01"
    formatDecimal 9.0009 `shouldBe` "9.00"
    formatDecimal 9.00009 `shouldBe` "9.00"
    formatDecimal 9.99 `shouldBe` "9.99"
    formatDecimal 9.999 `shouldBe` "10.00"
    formatDecimal 9.9999 `shouldBe` "10.00"

  it "formats numbers from 10 inclusive to 100 exclusive with percision of 1" $ do
    formatDecimal 10.0 `shouldBe` "10.0"
    formatDecimal 10.01 `shouldBe` "10.0"
    formatDecimal 99.0 `shouldBe` "99.0"
    formatDecimal 99.9 `shouldBe` "99.9"
    formatDecimal 99.99 `shouldBe` "100.0"

  it "formats numbers from 100 inclusive to 1000 exclusive with percision of 0" $ do
    formatDecimal 100.0 `shouldBe` "100"
    formatDecimal 100.1 `shouldBe` "100"
    formatDecimal 100.9 `shouldBe` "101"
    formatDecimal 999.0 `shouldBe` "999"
    formatDecimal 999.9 `shouldBe` "1000"
