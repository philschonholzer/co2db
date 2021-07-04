module Web.View.Layout (defaultLayout, Html, renderWeight, calcAmountFromBase, renderPer) where

import Data.Fixed
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

          <title>CO2 Database</title>
      </head>
      <body>
        <header class="page-header">
          <div class="container">
            <nav>
                <a class="navbar-brand" href="/">CO<sub>2</sub> Database</a>
                <a tabindex="0" class="menu-button" role="button" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
                  Menu
                </a>
                <ul class="collapse-items" id="navbarSupportedContent">
                    <li class="nav-item">
                      <a class="nav-link" href="/Co2Producers">CO<sub>2</sub> Producers</a>
                    </li>
                    <li class="nav-item">
                      <a class="nav-link" href="/about" tabindex="-1">About</a>
                    </li>
                      {loginLogoutButton}
                </ul>
            </nav>
          </div>
        </header>
        <main>
          <div class="container mt-4">
                {renderFlashMessages}
                {inner}
          </div>
        </main>
        <footer class="page-footer">
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
        Just user ->
          [hsx|
              <li class="nav-item dropdown">
                <a class="nav-link dropdown-toggle" href="#" id="navbarDropdown" role="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                  {get #email currentUser}
                </a>
                <div class="dropdown-menu" aria-labelledby="navbarDropdown">
                  <a class="dropdown-item nav-link" href={EditUserAction (get #id currentUser) }>Edit User</a>
                  <a class="js-delete js-delete-no-confirm dropdown-item nav-link" href={DeleteSessionAction}>Logout</a>
                </div>
              </li>
              |]
        Nothing ->
          [hsx|
              <li class="nav-item">
                <a class="nav-link" href={NewSessionAction}>Login</a>
              </li>
              <li class="nav-item">
                <a class="nav-link" href={NewUserAction}>Sign up</a>
              </li>
            |]

stylesheets :: Html
stylesheets =
  [hsx|
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

renderWeight :: Double -> Html
renderWeight weight = [hsx|<b>{weight `div'` 1000 |> show}</b><span>,{mod' weight 1000 |> threeDec} kg</span>|]

threeDec :: Double -> String
threeDec dec = dec |> printf "%03.0F"

calcAmountFromBase :: (?context :: ControllerContext) => Co2Producer' categoryId userId -> (Co2Producer' categoryId userId -> Double) -> H.Html
calcAmountFromBase co2Emitter consumption =
  co2Emitter
    |> get #gCo2e
    |> (/ per co2Emitter)
    |> (* consumption co2Emitter)
    |> renderWeight

renderPer :: Double -> String
renderPer amount 
          | amount `mod'` 1 == 0 = printf "%.f" amount
          | otherwise =  printf "%.2f" amount

