module Web.Routes where

import Generated.Types
import IHP.RouterPrelude
import Web.Types

instance HasPath StaticController where
  pathTo AboutAction = "/about"
  pathTo WelcomeAction = "/"

instance CanRoute StaticController where
  parseRoute' =
    (string "/" <* endOfInput >> pure WelcomeAction)
      <|> (string "/about" <* endOfInput >> pure AboutAction)

-- Generator Marker
instance AutoRoute Co2EmittersController

instance AutoRoute CategoriesController

instance AutoRoute SessionsController

instance AutoRoute UsersController
