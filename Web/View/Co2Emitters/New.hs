module Web.View.Co2Emitters.New where

import Web.View.Prelude

data NewView = NewView {co2Emitter :: Co2Emitter, categories :: [Category]}

instance View NewView where
  html NewView {..} =
    [hsx|
        <h1>New Co2Emitter</h1>
        {renderForm co2Emitter categories}
    |]

renderForm :: Co2Emitter -> [Category] -> Html
renderForm co2Emitter categories =
  formFor
    co2Emitter
    [hsx|
    {(textField #title) { required = True }}
    {(textField #description)}
    {(selectField #categoryId categories) { required = True }}
    {(textField #gCo2e) { required = True }}
    {(textField #per) { required = True }}
    {(selectField #unit $ enumFrom Kilometer ) { required = True }}
    {(textField #commonConsumption) { required = True }}
    {(textField #averageYearlyConsumption) { required = True }}
    {(textareaField #source) { required = True }}
    {(textField #image)}
    {submitButton}
|]
