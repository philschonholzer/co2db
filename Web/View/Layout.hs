module Web.View.Layout (defaultLayout, Html, renderWeight) where

import Generated.Types
import IHP.Controller.RequestContext
import IHP.Environment
import IHP.ViewPrelude
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf
import Web.Routes
import Web.Types

defaultLayout :: Html -> Html
defaultLayout inner =
  H.docTypeHtml ! A.lang "en" $
    [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>App</title>
</head>
<body>
  <header>
    <nav class="navbar navbar-expand-lg navbar-light bg-light">
      <div class="container">
        <a class="navbar-brand" href="/">CO<sub>2</sub> Database</a>
        <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
          <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse  justify-content-end" id="navbarSupportedContent">
          <ul class="navbar-nav">
            <li class="nav-item">
              <a class="nav-link" href="/Co2Emitters">Emitters</a>
            </li>
            <!-- <li class="nav-item dropdown">
              <a class="nav-link dropdown-toggle" href="#" id="navbarDropdown" role="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                Emitter by Sector
              </a>
              <div class="dropdown-menu" aria-labelledby="navbarDropdown">
                <a class="dropdown-item" href="#">Transportation</a>
                <a class="dropdown-item" href="#">Electricity</a>
                <a class="dropdown-item" href="#">Buildings</a>
                <a class="dropdown-item" href="#">Agricalture</a>
                <a class="dropdown-item" href="#">Industrie</a>
              </div>
            </li> -->
            <li class="ml-auto nav-item">
              <a class="nav-link" href="/about" tabindex="-1">About</a>
            </li>
            <li class="ml-auto nav-item">
              {loginLogoutButton}
            </li>
          </ul>
        </div>
      </div>
    </nav>
  </header>
  <main>
    <div class="container mt-4">
          {renderFlashMessages}
          {inner}
    </div>
  </main>
  <footer>
    <div class="container">
      <p>2021 CO<sub>2</sub> Database</p>
    </div>
  </footer>
</body>
|]
  where
    loginLogoutButton :: Html
    loginLogoutButton =
        case fromFrozenContext @(Maybe User) of
            Just user -> [hsx|
              <li class="nav-item dropdown">
                <a class="nav-link dropdown-toggle" href="#" id="navbarDropdown" role="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                  {get #email currentUser}
                </a>
                <div class="dropdown-menu" aria-labelledby="navbarDropdown">
                  <a class="dropdown-item" href={EditUserAction (get #id currentUser) }>Edit User</a>
                  <a class="js-delete js-delete-no-confirm dropdown-item" href={DeleteSessionAction}>Logout</a>
                </div>
              </li>
              |]
            Nothing -> [hsx|<a class="nav-link" href={NewSessionAction}>Login</a>|]

stylesheets :: Html
stylesheets =
  [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    |]

scripts :: Html
scripts =
  [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/jquery-3.2.1.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
        <script src="/vendor/turbolinks.js"></script>
        <script src="/vendor/turbolinksInstantClick.js"></script>
        <script src="/vendor/turbolinksMorphdom.js"></script>
        <script src="/helpers.js"></script>
        <script src="/ihp-auto-refresh.js"></script>
    |]

metaTags :: Html
metaTags =
  [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="App"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="TODO"/>
    <meta property="og:description" content="TODO"/>
    {autoRefreshMeta}
|]

instance CanSelect Category where
  -- Here we specify that the <option> value should contain a `Id User`
  type SelectValue Category = Id Category

  -- Here we specify how to transform the model into <option>-value
  selectValue = get #id

  -- And here we specify the <option>-text
  selectLabel = get #title

instance CanSelect Unit where
  type SelectValue Unit = Unit
  selectValue value = value

  selectLabel = tshow

renderWeight :: Int -> Html
renderWeight weight = [hsx|{weight `div` 1000}<span class="text-muted"><small class="text-muted">,{mod weight 1000 |> threeDec}</small> kg</span>|]

threeDec :: Integral a => a -> String
threeDec dec = (fromIntegral dec :: Int) |> printf "%03d"
