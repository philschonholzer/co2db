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
        <title>{headTitle}</title>
        {metaTags}
        {stylesheets}
        {scripts}
        {favicon}
      </head>
      <body>
        <header class="page-header">
          <div class="container">
            <nav>
                <a class="navbar-brand" href="/">CO<sub>2</sub> Data</a>
                <a tabindex="0" class="menu-button" role="button" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
                  Menu
                </a>
                <ul class="collapse-items" id="navbarSupportedContent">
                    <li class="nav-item">
                      <a class="nav-link" href={Co2ProducersAction}>CO<sub>2</sub> Contributors</a>
                    </li>
                    <li class="nav-item">
                      <a class="nav-link" href={AboutAction} tabindex="-1">About</a>
                    </li>
                      {loginLogoutButton}
                </ul>
            </nav>
          </div>
        </header>
        {renderFlashMessages}
        <main>
          <div class="container mt-4">
                {inner}
          </div>
        </main>
        <footer class="page-footer">
          <div class="container">
            <p>2022&ensp;•&ensp;co2data.org</p>
          </div>
        </footer>
      </body>
    |]
  where
    headTitle :: Text
    headTitle = case pageTitleOrNothing of
      Nothing -> "CO2 Data"
      Just title -> title <> " | CO2 Data"

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
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts =
  [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/jquery-3.6.0.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
        <script src="/vendor/turbolinks.js"></script>
        <script src="/vendor/turbolinksInstantClick.js"></script>
        <script src="/vendor/turbolinksMorphdom.js"></script>
        <script src="/vendor/ihp-ssc.js"></script>
        <script src="/helpers.js"></script>
        <script src="/ihp-auto-refresh.js"></script>
    |]

metaTags :: Html
metaTags =
  [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    {descriptionOrDefault "What are the CO₂ emissions of things."}
    {ogTitleOrDefault "CO2 Data"}
    {ogDescriptionOrDefault "What are the CO₂ emissions of things."}
    {ogTypeOrDefault "website"}
    {ogImage}
    {ogUrl}

    {autoRefreshMeta}
|]

favicon :: Html 
favicon = 
  [hsx|
    <link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">
    <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
    <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
    <link rel="manifest" href="/site.webmanifest">
    <link rel="mask-icon" href="/safari-pinned-tab.svg" color="#007bff">
    <meta name="msapplication-TileColor" content="#007bff">
    <meta name="theme-color" content="#007bff">
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
renderWeight weight = 
  [hsx|
    <span class="weight-integer-part">{weight `div'` 1000 |> show}</span>
    <span class="weight-fractional-part">.{weight `mod'` 1000 |> (printf "%03.0F" :: Double -> String)}</span>kg
  |]

calcAmountFromBase :: (?context :: ControllerContext) => Source' co2ProducerId userId -> (Source' co2ProducerId userId -> Double) -> H.Html
calcAmountFromBase source consumption =
  source
    |> get #gCo2e
    |> (/ get #per source)
    |> (* consumption source)
    |> renderWeight

renderPer :: Double -> String
renderPer amount 
          | amount `mod'` 1 == 0 = printf "%.f" amount
          | otherwise =  printf "%.2f" amount

