module Web.Routes where

import Generated.Types
import IHP.RouterPrelude
import Web.Types

instance HasPath StaticController where
  pathTo AboutAction = "/about"
  pathTo HowToContributeAction = "/how-to-contribute"

instance CanRoute StaticController where
  parseRoute' =
    (string "/how-to-contribute" <* endOfInput >> pure HowToContributeAction)
      <|> (string "/about" <* endOfInput >> pure AboutAction)

-- Generator Marker
instance AutoRoute Co2ProducersController

instance AutoRoute Co2ProducerDetailsController

instance AutoRoute CategoriesController

instance AutoRoute SessionsController

instance AutoRoute UsersController
