module Web.Routes where

import Generated.Types
import IHP.RouterPrelude
import Web.Types

instance HasPath StaticController where
  pathTo AboutAction = "/about"
  pathTo HowToContributeAction = "/how-to-contribute"

instance HasPath Co2ProducersController where
  pathTo (ShowCo2ProducerAction _ (Just slug)) = "/producers/" <> slug
  pathTo (ShowCo2ProducerAction (Just id) _) = "/producers/" <> tshow id
  pathTo (EditCo2ProducerAction (Just id) _) = "/producers/" <> tshow id <> "/edit"
  pathTo (UpdateCo2ProducerAction (Just id) _) = "/producers/" <> tshow id <> "/update"
  pathTo (DeleteCo2ProducerAction (Just id) _) = "/producers/" <> tshow id <> "/delete"
  pathTo NewCo2ProducerAction = "/producers/new"
  pathTo CreateCo2ProducerAction = "/producers/CreateCo2Producer"
  pathTo _ = "/producers/"

instance CanRoute StaticController where
  parseRoute' =
    (string "/how-to-contribute" <* endOfInput >> pure HowToContributeAction)
      <|> (string "/about" <* endOfInput >> pure AboutAction)

instance CanRoute Co2ProducersController where
  parseRoute' = do
    string "/producers/"
    let co2ProducerById = do
          id <- parseId
          endOfInput
          pure ShowCo2ProducerAction {co2ProducerId = Just id, slug = Nothing}

        co2ProducerBySlug = do
          slug <- parseText
          pure ShowCo2ProducerAction {co2ProducerId = Nothing, slug = Just slug}

        co2ProducerUpdate = do
          id <- parseId
          string "/update"
          endOfInput
          pure UpdateCo2ProducerAction {co2ProducerId = Just id, slug = Nothing}

        co2ProducerEdit = do
          id <- parseId
          string "/edit"
          endOfInput
          pure EditCo2ProducerAction {co2ProducerId = Just id, slug = Nothing}

        co2ProducerDelete = do
          id <- parseId
          string "/delete"
          endOfInput
          pure DeleteCo2ProducerAction {co2ProducerId = Just id, slug = Nothing}

        co2ProducerRoot = do
          endOfInput
          pure Co2ProducersAction

        co2ProducerNew = do
          string "new"
          pure NewCo2ProducerAction

        co2ProducerCreate = do
          string "CreateCo2Producer"
          pure CreateCo2ProducerAction

    co2ProducerRoot <|> co2ProducerUpdate <|> co2ProducerEdit <|> co2ProducerDelete <|> co2ProducerNew <|> co2ProducerCreate <|> co2ProducerById <|> co2ProducerBySlug

-- Generator Marker
instance AutoRoute SourcesController

instance AutoRoute CategoriesController

instance AutoRoute SessionsController

instance AutoRoute UsersController
