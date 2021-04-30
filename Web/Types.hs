module Web.Types where

import Generated.Types
import IHP.LoginSupport.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController
  = AboutAction
  | WelcomeAction
  deriving (Eq, Show, Data)

data Co2EmittersController
  = Co2EmittersAction
  | NewCo2EmitterAction
  | ShowCo2EmitterAction {co2EmitterId :: !(Id Co2Emitter)}
  | CreateCo2EmitterAction
  | EditCo2EmitterAction {co2EmitterId :: !(Id Co2Emitter)}
  | UpdateCo2EmitterAction {co2EmitterId :: !(Id Co2Emitter)}
  | DeleteCo2EmitterAction {co2EmitterId :: !(Id Co2Emitter)}
  deriving (Eq, Show, Data)

data CategoriesController
  = CategoriesAction
  | NewCategoryAction
  | ShowCategoryAction {categoryId :: !(Id Category)}
  | CreateCategoryAction
  | EditCategoryAction {categoryId :: !(Id Category)}
  | UpdateCategoryAction {categoryId :: !(Id Category)}
  | DeleteCategoryAction {categoryId :: !(Id Category)}
  deriving (Eq, Show, Data)

instance HasNewSessionUrl User where
  newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data SessionsController
  = NewSessionAction
  | CreateSessionAction
  | DeleteSessionAction
  deriving (Eq, Show, Data)

data UsersController
  = NewUserAction
  | ShowUserAction {userId :: !(Id User)}
  | CreateUserAction
  | EditUserAction {userId :: !(Id User)}
  | UpdateUserAction {userId :: !(Id User)}
  | DeleteUserAction {userId :: !(Id User)}
  deriving (Eq, Show, Data)
