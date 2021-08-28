module Web.Types where

import Generated.Types
import IHP.LoginSupport.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController
  = AboutAction
  | HowToContributeAction
  deriving (Eq, Show, Data)

data Co2ProducersController
  = Co2ProducersAction
  | -- | SearchCo2ProducersAction
    NewCo2ProducerAction
  | ShowCo2ProducerAction {co2ProducerId :: !(Id Co2Producer)}
  | CreateCo2ProducerAction
  | EditCo2ProducerAction {co2ProducerId :: !(Id Co2Producer)}
  | UpdateCo2ProducerAction {co2ProducerId :: !(Id Co2Producer)}
  | DeleteCo2ProducerAction {co2ProducerId :: !(Id Co2Producer)}
  deriving (Eq, Show, Data)

data SourcesController
  = SourcesAction
  | NewSourceAction {co2ProducerId :: !(Id Co2Producer)}
  | ShowSourceAction {sourceId :: !(Id Source)}
  | CreateSourceAction
  | EditSourceAction {sourceId :: !(Id Source)}
  | UpdateSourceAction {sourceId :: !(Id Source)}
  | DeleteSourceAction {sourceId :: !(Id Source)}
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
