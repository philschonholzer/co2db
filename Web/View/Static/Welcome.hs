module Web.View.Static.Welcome where

import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
  html WelcomeView =
    [hsx|
      <h1>CO<sub>2</sub> Database</h1>
      <p>What are the CO<sub>2</sub> emissions of things.</p> 
      <a class="btn btn-primary" role="button" href="/Co2Emitters">Show Emitters</a> 
    |]
