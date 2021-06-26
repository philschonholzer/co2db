module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.About
import Web.View.Static.HowToContribute

instance Controller StaticController where
  action HowToContributeAction = render HowToContributeView
  action AboutAction = render AboutView
