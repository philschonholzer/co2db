module Test.Web.RoutesSpec where

import qualified Data.Attoparsec.ByteString as Attoparsec
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Web.FrontController
import Web.Types

tests = beforeAll (mockContextNoDatabase WebApplication (pure ())) do
  describe "Web.Routes" do
    describe "Co2ProducerController" do
      it "should be available at /producers" $ withContext do
        "/producers/" `shouldRouteTo` Co2ProducersAction

      it "should be available at /producers/new" $ withContext do
        "/producers/new" `shouldRouteTo` NewCo2ProducerAction

      it "should be available at /producers" $ withContext do
        "/producers/CreateCo2Producer" `shouldRouteTo` CreateCo2ProducerAction

      it "should be available at /producers/beef" $ withContext do
        "/producers/beef" `shouldRouteTo` ShowCo2ProducerAction {co2ProducerId = Nothing, slug = Just "beef"}

      it "should be available at /producers/0bca60db-571e-4cdd-b02a-8d5b9e7e6295" $ withContext do
        "/producers/0bca60db-571e-4cdd-b02a-8d5b9e7e6295" `shouldRouteTo` ShowCo2ProducerAction {co2ProducerId = Just "0bca60db-571e-4cdd-b02a-8d5b9e7e6295", slug = Nothing}

      it "should be available at /producers/0bca60db-571e-4cdd-b02a-8d5b9e7e6295/edit" $ withContext do
        "/producers/0bca60db-571e-4cdd-b02a-8d5b9e7e6295/edit" `shouldRouteTo` EditCo2ProducerAction {co2ProducerId = Just "0bca60db-571e-4cdd-b02a-8d5b9e7e6295", slug = Nothing}

      it "should be available at /producers/0bca60db-571e-4cdd-b02a-8d5b9e7e6295/update" $ withContext do
        "/producers/0bca60db-571e-4cdd-b02a-8d5b9e7e6295/update" `shouldRouteTo` UpdateCo2ProducerAction {co2ProducerId = Just "0bca60db-571e-4cdd-b02a-8d5b9e7e6295", slug = Nothing}

      it "should be available at /producers/0bca60db-571e-4cdd-b02a-8d5b9e7e6295/delete" $ withContext do
        "/producers/0bca60db-571e-4cdd-b02a-8d5b9e7e6295/delete" `shouldRouteTo` DeleteCo2ProducerAction {co2ProducerId = Just "0bca60db-571e-4cdd-b02a-8d5b9e7e6295", slug = Nothing}

shouldRouteTo path action = Attoparsec.parseOnly parseRoute' path `shouldBe` Right action
