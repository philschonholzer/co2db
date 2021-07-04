module Web.View.Sessions.New where

import IHP.AuthSupport.View.Sessions.New
import Web.View.Prelude

instance View (NewView User) where
  html NewView {..} =
    [hsx|
        <div id="sessions-new">
          <header>
            <h1>Login</h1>
          </header>
          {renderForm user}
          <p style="margin-top: 2em;" >Not registered? <a href={NewUserAction}>Sign up</a></p> 
        </div>
    |]

renderForm :: User -> Html
renderForm user =
  [hsx|
    <form method="POST" action={CreateSessionAction}>
        <div class="form-group">
            <input name="email" value={get #email user} type="email" class="form-control" placeholder="E-Mail" required="required" autofocus="autofocus" />
        </div>
        <div class="form-group">
            <input name="password" type="password" class="form-control" placeholder="Password"/>
        </div>
        <button type="submit" class="btn btn-primary btn-block">Login</button>
    </form>
|]
