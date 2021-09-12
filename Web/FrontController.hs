module Web.FrontController where

import IHP.LoginSupport.Middleware
import IHP.RouterPrelude
import IHP.ServerSideComponent.RouterFunctions
import Web.Component.CommonConsumption
import Web.Controller.Categories
import Web.Controller.Co2Producers
import Web.Controller.Sources
import Web.Controller.Prelude
import Web.Controller.Sessions
import Web.Controller.Static
import Web.Controller.Users
import Web.View.Layout (defaultLayout)

instance FrontController WebApplication where
  controllers =
    [ startPage Co2ProducersAction ,
      parseRoute @StaticController,
      parseRoute @SessionsController,
      -- Generator Marker
      parseRoute @UsersController,
      parseRoute @CategoriesController,
      parseRoute @Co2ProducersController,
      parseRoute @SourcesController,
      routeComponent @CommonConsumption
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh
    initAuthentication @User
