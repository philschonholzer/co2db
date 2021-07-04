module Web.View.Static.HowToContribute where

import Web.View.Prelude

data HowToContributeView = HowToContributeView

instance View HowToContributeView where
  html HowToContributeView =
    [hsx|
      <header>
        <h1>Contribution</h1> 
      </header>
      <p>I would love for you to contribute to the CO<sub>2</sub> database. Either by contributing of emitters or by improving the database. You can find the project on Github.</p> 
    |]
