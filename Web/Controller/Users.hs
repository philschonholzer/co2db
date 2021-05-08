module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.New
import Web.View.Users.Edit
import Web.View.Users.Show

instance Controller UsersController where
    action NewUserAction = do
        let user = newRecord
        render NewView { .. }

    action ShowUserAction { userId } = do
        fullUser <- fetch userId
        let user = set #passwordHash "" fullUser
        accessDeniedUnless (get #id user == currentUserId)
        render ShowView { .. }

    action EditUserAction { userId } = do
        fullUser <- fetch userId
        let user = set #passwordHash "" fullUser
        accessDeniedUnless (get #id user == currentUserId)
        render EditView { .. }

    action UpdateUserAction { userId } = do
        user <- fetch userId
        accessDeniedUnless (get #id user == currentUserId)
        user
            |> buildUser
            |> validateIsUnique #email
            >>= ifValid \case
                Left user -> render EditView { .. }
                Right user -> do
                    hashed <- hashPassword (get #passwordHash user)
                    user <- user
                        |> set #passwordHash hashed
                        |> updateRecord
                    setSuccessMessage "User updated"
                    redirectTo EditUserAction { .. }

    action CreateUserAction = do
        let user = newRecord @User
        user
          |> buildUser
          |> validateIsUnique #email
          >>= ifValid \case
            Left user -> render NewView { .. } 
            Right user -> do
              hashed <- hashPassword (get #passwordHash user)
              user <- user
                  |> set #passwordHash hashed
                  |> createRecord
              setSuccessMessage "You have registered successfully"
              redirectTo WelcomeAction

    action DeleteUserAction { userId } = do
        user <- fetch userId
        accessDeniedUnless (get #id user == currentUserId)
        deleteRecord user
        setSuccessMessage "User deleted"
        redirectTo WelcomeAction

buildUser user = user
    |> fill @["email","passwordHash","failedLoginAttempts"]
    |> validateField #email isEmail
    |> validateField #passwordHash nonEmpty
