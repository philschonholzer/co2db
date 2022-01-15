module Test.Web.RoutesSpec where

import IHP.Prelude
import IHP.RouterPrelude
import Test.Hspec
import Web.Types
import Web.View.Prelude

tests :: Spec
tests = describe "Routes" $ do
  it "generates correct path for empty route" do
    pathTo Co2ProducersAction `shouldBe` "/producers/"
    pathTo NewCo2ProducerAction `shouldBe` "/producers/new"
    pathTo CreateCo2ProducerAction `shouldBe` "/producers/CreateCo2Producer"
    pathTo (ShowCo2ProducerAction Nothing (Just "beef")) `shouldBe` "/producers/beef"
    pathTo (EditCo2ProducerAction (Just (Id "eb61267e-17db-4709-b7ee-bb55252c3c22")) Nothing) `shouldBe` "/producers/eb61267e-17db-4709-b7ee-bb55252c3c22/edit"
    pathTo (UpdateCo2ProducerAction (Just (Id "eb61267e-17db-4709-b7ee-bb55252c3c22")) Nothing) `shouldBe` "/producers/eb61267e-17db-4709-b7ee-bb55252c3c22/update"
    pathTo (DeleteCo2ProducerAction (Just (Id "eb61267e-17db-4709-b7ee-bb55252c3c22")) Nothing) `shouldBe` "/producers/eb61267e-17db-4709-b7ee-bb55252c3c22/delete"
