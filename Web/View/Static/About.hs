module Web.View.Static.About where

import Web.View.Prelude

data AboutView = AboutView

instance View AboutView where
  html AboutView =
    [hsx|
      <h1>About CO2 Database</h1>
      <p>What are the CO₂ emissions of things.</p> 
      <p>How can we as a society fight the climate crisis if we hardly know the opponent (CO₂). 
        With the CO₂ database I want to show the different CO₂ emitters and their emissions. 
        Everyone should be able to know and understand what causes how much CO₂.</p>

      <h2>Contribution</h2>
      <p>I would love for you to contribute to the CO₂ database. Either by contributing of emitters or by improving the database. You can find the project on Github.</p> 

      <h2>About me</h2>
      <p>My name is Philip Schönholzer. Get in touch by <a href="https://twitter.com/pschonholzer">Twitter</a> or <a href="https://www.reddit.com/user/phischer_h">Reddit</a></p>.
    |]
