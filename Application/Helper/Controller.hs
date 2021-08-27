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

validateWithOtherField ::
  ( SetField "meta" model MetaBag,
    KnownSymbol field,
    KnownSymbol name,
    HasField field model fieldValue,
    HasField name model t,
    HasField "meta" model MetaBag
  ) =>
  Proxy field ->
  (t -> Validator fieldValue) ->
  Proxy name ->
  model ->
  model
validateWithOtherField field validateFunction validateWithField record = validateField field (validateFunction $ get validateWithField record) record

validateWithOtherFields ::
  ( SetField "meta" model MetaBag,
    KnownSymbol field,
    KnownSymbol name1,
    KnownSymbol name2,
    HasField field model fieldValue,
    HasField name1 model a,
    HasField name2 model b,
    HasField "meta" model MetaBag
  ) =>
  Proxy field ->
  ((a, b) -> Validator fieldValue) ->
  Proxy name1 ->
  Proxy name2 ->
  model ->
  model
validateWithOtherFields field validateFunction validateWithField1 validateWithField2 record = validateField field (validateFunction (get validateWithField1 record, get validateWithField2 record)) record
