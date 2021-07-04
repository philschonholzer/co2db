module Web.View.Co2Emitters.Edit where

import Web.View.Prelude

data EditView = EditView {co2Emitter :: Co2Emitter, categories :: [Category]}

instance View EditView where
  html EditView {..} =
    [hsx|
        <header>
          <h1>Edit Producer</h1>
        </header>
        <section>{renderForm co2Emitter categories}</section>
    |]

renderForm :: Co2Emitter -> [Category] -> Html
renderForm co2Emitter categories =
  formFor
    co2Emitter
    [hsx|
        {(textField #title)}
        {(textField #description)}
        {(selectField #categoryId categories)}
        {(textField #gCo2e)}
        {(textField #per)}
        {(selectField #unit $ enumFrom Kilometer)}
        {(textField #commonConsumption)}
        {(textField #averageYearlyConsumption)}
        {(textareaField #source)}
        <p>Please provide the precise text from the source link. Best is to add 
          the text to the end of the link like this: 
          https://sourcedomain.org/somelink<b>#:~:text=This%20is%20the%20text%20I%20want%20to%20quote.</b></p>
        <p>Please also provide the original value from the source and show how 
          you converted it to the needed format. For ex. if the source is in 
          pounds and you converted it to grams the source should contain a following 
          description:</p>
        <p>Original source: 0.92 pounds of CO2 emissions per kWh</p>
        <p>Conversion: 0.92 pounds / kWh × 454 = 417.305 gramm / kWh<br /> 417.305 gramms / kWh × 0.007 = 2.921135 gramms / 7 Wh</p>
        {(textField #image)}
        {submitButton}
    |]
