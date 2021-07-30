module Web.View.Co2Producers.Edit where

import Web.View.Prelude

data EditView = EditView {co2Producer :: Co2Producer, categories :: [Category]}

instance View EditView where
  html EditView {..} =
    [hsx|
        <header>
          <nav>
            <ol class="breadcrumb">
              <li class="breadcrumb-item"><a href={Co2ProducersAction}>Producers</a></li>
              <li class="breadcrumb-item"><a href={ShowCo2ProducerAction (get #id co2Producer)}>{get #title co2Producer}</a> </li>
              <li class="breadcrumb-item active">Edit {get #title co2Producer}</li>
            </ol>
          </nav>

          <h1>Edit Producer</h1>
        </header>
        <section>{renderForm co2Producer categories}</section>
    |]

renderForm :: Co2Producer -> [Category] -> Html
renderForm co2Producer categories =
  formFor
    co2Producer
    [hsx|
        {(textField #title)}
        {(textField #description)}
        {(selectField #categoryId categories)}
        {(textField #gCo2e) { fieldLabel = "Grams (g) CO₂e"}}
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
