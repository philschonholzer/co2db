module Config where

import IHP.Environment
import IHP.FrameworkConfig
import IHP.Prelude

config :: ConfigBuilder
config = do
  option Development
  option (AppHostname "localhost")

adminEmail :: Text
adminEmail = "phi.sch@hotmail.ch"
