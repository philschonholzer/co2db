module Web.View.Users.New where

import Web.View.Prelude

data NewView = NewView {user :: User}

instance View NewView where
  html NewView {..} =
    [hsx|
        <div id="sessions-new">
          <header>
            <h1>Sign up</h1>
          </header>
          {renderForm user}
        </div>
    |]

renderForm :: User -> Html
renderForm user =
  [hsx|
    <form method="POST" action={CreateUserAction}>
        <div class="form-group">
            <input name="email" value={get #email user} type="email" class="form-control" placeholder="E-Mail" required="required" autofocus="autofocus" />
        </div>
        <div class="form-group">
            <input name="passwordHash" type="password" class="form-control" placeholder="Password"/>
        </div>
        <button type="submit" class="btn btn-primary btn-block">Sign up</button>
    </form>
|]
