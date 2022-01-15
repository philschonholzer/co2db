module Test.Web.RoutesSpec where

import IHP.Prelude
import IHP.RouterPrelude
import Test.Hspec
import Web.Types
import Web.View.Prelude

tests :: Spec
tests = describe "Routes" $ do
  describe "CO2 Producers" do
    it "generates correct path for root" do
      pathTo Co2ProducersAction `shouldBe` "/producers/"
    it "generates correct path for new" do
      pathTo NewCo2ProducerAction `shouldBe` "/producers/new"
    it "generates correct path for create" do
      pathTo CreateCo2ProducerAction `shouldBe` "/producers/CreateCo2Producer"
    it "generates correct path for show" do
      pathTo (ShowCo2ProducerAction Nothing (Just "beef")) `shouldBe` "/producers/beef"
    it "generates correct path for edit" do
      pathTo (EditCo2ProducerAction (Just (Id "eb61267e-17db-4709-b7ee-bb55252c3c22")) Nothing) `shouldBe` "/producers/eb61267e-17db-4709-b7ee-bb55252c3c22/edit"
    it "generates correct path for update" do
      pathTo (UpdateCo2ProducerAction (Just (Id "eb61267e-17db-4709-b7ee-bb55252c3c22")) Nothing) `shouldBe` "/producers/eb61267e-17db-4709-b7ee-bb55252c3c22/update"
    it "generates correct path for delete" do
      pathTo (DeleteCo2ProducerAction (Just (Id "eb61267e-17db-4709-b7ee-bb55252c3c22")) Nothing) `shouldBe` "/producers/eb61267e-17db-4709-b7ee-bb55252c3c22/delete"
