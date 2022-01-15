module Test.Web.Controller.Co2ProducersSpec where

import Config
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Network.HTTP.Types.Status
import Test.Hspec
import Web.FrontController ()
import Web.Types
import Network.Wai (responseHeaders)
import BasicPrelude (encodeUtf8)
import Debug.Trace

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "Co2ProducerController" $ do
    it "has no existing producers" $ withContext do
      count <- query @Co2Producer
          |> fetchCount
      count `shouldBe` 0

    it "calling NewCo2ProducerAction will render a new form" $ withContext do
      mockActionStatus NewCo2ProducerAction `shouldReturn` status302
      mockActionStatus Co2ProducersAction `shouldReturn` status200

    it "creates a new co2 producer" $ withContext do
      -- Create a user for our test case
      user <- newRecord @User
        |> set #email "philip@co2db.org"
        |> createRecord

      -- Create a category for our test case
      category <- newRecord @Category
        |> set #title "New Category"
        |> createRecord


      -- Log into the user and then call CreateCo2ProducerAction
      response <- withUser user do
        callActionWithParams CreateCo2ProducerAction [("title", "Beef"), ("description", "Description of co2 producer"), ("categoryId", encodeUtf8 $ tshow $ get #id category), ("userId", encodeUtf8 $ tshow $ get #id user)]

      let (Just location) = lookup "Location" (responseHeaders response)
      location `shouldBe` "http://localhost:8000/producers/"

      -- Only one co2 producer should exist.
      count <- query @Co2Producer |> fetchCount
      count `shouldBe` 1

      co2Producer <- query @Co2Producer |> fetchOne

      get #title co2Producer `shouldBe` "Beef"
      get #description co2Producer `shouldBe` "Description of co2 producer"
      get #categoryId co2Producer `shouldBe` get #id category


      -- Create a second not unique co2 producer
      responseNotUnique <- withUser user do
          callActionWithParams CreateCo2ProducerAction [("title", "Beef"), ("description", "Description of co2 producer"), ("categoryId", encodeUtf8 $ tshow $ get #id category), ("userId", encodeUtf8 $ tshow $ get #id user)]

      responseNotUnique `responseStatusShouldBe` status200
      responseNotUnique `responseBodyShouldContain` "producer with this slug exists already."

    it "can show co2 producer" $ withContext do
      -- Query a user for our test case
      user <- query @User |> fetchOne

      -- Query a category for our test case
      category <- query @Category |> fetchOne

      co2Producer <- newRecord @Co2Producer
        |> set #title "Lorem Ipsum"
        |> set #description "**Mark down**"
        |> set #categoryId (get #id category)
        |> set #userId (get #id user)
        |> createRecord

      response <- callAction ShowCo2ProducerAction { co2ProducerId = Just (get #id co2Producer), slug = Nothing }

      response `responseStatusShouldBe` status200
      response `responseBodyShouldContain` "Lorem Ipsum"

      -- For debugging purposes you could do the following, to
      -- see the HTML printed out on the terminal.
      --
      -- body <- responseBody response
      -- traceShowM body
      --
      -- or
      --
      -- body <- responseBody response
      -- putStrLn (cs body)