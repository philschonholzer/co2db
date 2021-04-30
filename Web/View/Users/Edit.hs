module Web.View.Users.Edit where

import Web.View.Prelude

data EditView = EditView {user :: User}

instance View EditView where
  html EditView {..} =
    [hsx|
        <h1>Edit User</h1>
        {renderForm user}
    |]

renderForm :: User -> Html
renderForm user =
  formFor
    user
    [hsx|
    {(textField #email)}
    {(textField #passwordHash)  { fieldLabel = "Password"}}
    {submitButton}
|]
