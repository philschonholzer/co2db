module Test.Web.PathsSpec where

import qualified Data.Attoparsec.ByteString as Attoparsec
import IHP.ControllerPrelude
import IHP.Prelude
import IHP.RouterPrelude
import IHP.Test.Mocking
import Test.Hspec
import Web.FrontController
import Web.Types
import Web.View.Prelude

tests :: Spec
tests =
  describe "Web.Paths" do
    describe "CO2 Producers" do
      it "generates correct path for root" do
        pathTo Co2ProducersAction `shouldBe` "/c/"
      it "generates correct path for new" do
        pathTo NewCo2ProducerAction `shouldBe` "/c/new"
      it "generates correct path for create" do
        pathTo CreateCo2ProducerAction `shouldBe` "/c/CreateCo2Producer"
      it "generates correct path for show (slug)" do
        pathTo (ShowCo2ProducerAction Nothing (Just "beef")) `shouldBe` "/c/beef"
      it "generates correct path for show (id)" do
        pathTo (ShowCo2ProducerAction (Just "0bca60db-571e-4cdd-b02a-8d5b9e7e6295") Nothing) `shouldBe` "/c/0bca60db-571e-4cdd-b02a-8d5b9e7e6295"
      it "generates correct path for edit" do
        pathTo (EditCo2ProducerAction (Just (Id "eb61267e-17db-4709-b7ee-bb55252c3c22")) Nothing) `shouldBe` "/c/eb61267e-17db-4709-b7ee-bb55252c3c22/edit"
      it "generates correct path for update" do
        pathTo (UpdateCo2ProducerAction (Just (Id "eb61267e-17db-4709-b7ee-bb55252c3c22")) Nothing) `shouldBe` "/c/eb61267e-17db-4709-b7ee-bb55252c3c22/update"
      it "generates correct path for delete" do
        pathTo (DeleteCo2ProducerAction (Just (Id "eb61267e-17db-4709-b7ee-bb55252c3c22")) Nothing) `shouldBe` "/c/eb61267e-17db-4709-b7ee-bb55252c3c22/delete"
