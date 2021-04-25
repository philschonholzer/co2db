module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.About
import Web.View.Static.Welcome

instance Controller StaticController where
  action WelcomeAction = render WelcomeView
  action AboutAction = render AboutView
