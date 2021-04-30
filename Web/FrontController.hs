module Web.FrontController where

import IHP.RouterPrelude
-- Controller Imports
import Web.Controller.Users
import Web.Controller.Categories
import Web.Controller.Co2Emitters
import Web.Controller.Prelude
import Web.Controller.Static
import Web.View.Layout (defaultLayout)

import IHP.LoginSupport.Middleware
import Web.Controller.Sessions

instance FrontController WebApplication where
  controllers =
    [ startPage WelcomeAction,
      parseRoute @StaticController,
      parseRoute @SessionsController,
      -- Generator Marker
      parseRoute @UsersController,
      parseRoute @CategoriesController,
      parseRoute @Co2EmittersController
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh
    initAuthentication @User
