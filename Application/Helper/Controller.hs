module Application.Helper.Controller where

import qualified Config
import IHP.Controller.Context
import IHP.Controller.Redirect
import IHP.ControllerPrelude

ensureisAdminUser :: (HasField "email" model Text, ?context :: ControllerContext, HasPath action) => model -> action -> IO ()
ensureisAdminUser currentUser action = case currentUser of
  user | get #email user == Config.adminEmail -> pure ()
  _ -> do
    setErrorMessage "Only admins are allowed to access this page"
    redirectTo action
    error "Unreachable"
