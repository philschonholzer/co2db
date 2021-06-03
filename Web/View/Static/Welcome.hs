module Web.View.Static.Welcome where

import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
  html WelcomeView =
    [hsx|
      <h1>CO<sub>2</sub> Database</h1>
      <p>How much CO<sub>2</sub> do things produce.</p> 
      <a class="btn btn-primary" role="button" href="/Co2Emitters">Show CO<sub>2</sub> Producers</a> 
    |]
