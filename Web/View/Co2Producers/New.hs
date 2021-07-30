module Web.View.Co2Producers.New where

import Web.View.Prelude

data NewView = NewView {co2Producer :: Co2Producer, categories :: [Category]}

instance View NewView where
  html NewView {..} =
    [hsx|
        <header>
          <nav>
            <ol class="breadcrumb">
              <li class="breadcrumb-item"><a href={Co2ProducersAction}>Producers</a></li>
              <li class="breadcrumb-item active">New {get #title co2Producer}</li>
            </ol>
          </nav>

          <h1>New Co2Producer</h1>
        </header>
        <section>{renderForm co2Producer categories}</section>
    |]

renderForm :: Co2Producer -> [Category] -> Html
renderForm co2Producer categories =
  formFor
    co2Producer
    [hsx|
    {(textField #title) { required = True }}
    {(textField #description)}
    {(selectField #categoryId categories) { required = True }}
    {(textField #gCo2e) { required = True, fieldLabel = "Grams (g) CO₂e"}}
    {(textField #per) { required = True }}
    {(selectField #unit $ enumFrom Kilometer ) { required = True }}
    {(textField #commonConsumption) { required = True }}
    {(textField #averageYearlyConsumption) { required = True }}
    {(textareaField #source) { required = True }}
    {(textField #image)}
    {submitButton}
|]
