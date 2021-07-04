module Web.View.Users.Show where

import Web.View.Prelude

data ShowView = ShowView {user :: User}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <header>
          <h1>Show User</h1>
        </header>
        <p>{user}</p>
    |]
