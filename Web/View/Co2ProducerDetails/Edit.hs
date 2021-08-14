module Web.View.Co2ProducerDetails.Edit where

import Web.View.Prelude

data EditView = EditView {co2ProducerDetail :: Co2ProducerDetail, co2Producer :: Co2Producer}

instance View EditView where
  html EditView {..} =
    [hsx|
        <header>
          <nav>
            <ol class="breadcrumb">
              <li class="breadcrumb-item"><a href={Co2ProducersAction}>Producers</a></li>
              <li class="breadcrumb-item"><a href={ShowCo2ProducerAction $ get #id co2Producer}>CO<sub>2</sub>{get #title co2Producer}</a> </li>
              <li class="breadcrumb-item active">Edit Source</li>
            </ol>
          </nav>
          <h1>Edit Source {fromMaybe "" $ get #region co2ProducerDetail} {get #year co2ProducerDetail}</h1>
        </header>
        <section>{renderForm co2ProducerDetail}</section>
    |]

renderForm :: Co2ProducerDetail -> Html
renderForm co2ProducerDetail =
  formFor
    co2ProducerDetail
    [hsx|
    {(hiddenField #co2ProducerId)}
    {(textField #region)}
    {(textField #year)}
    {(textField #gCo2e)}
    {(textField #commonConsumption)}
    {(textField #averageYearlyConsumption)}
    {(textField #per)}
    {(textField #unit)}
    {(textField #source)}
    <p>Please provide the precise text from the source link. Best is to add 
      the text to the end of the link like this: 
      https://sourcedomain.org/somelink<b>#:~:text=This%20is%20the%20text%20I%20want%20to%20quote.</b></p>
    <p>Please also provide the original value from the source and show how 
      you converted it to the needed format. For ex. if the source is in 
      pounds and you converted it to grams the source should contain a following 
      description:</p>
    <p>Original source: 0.92 pounds of CO2 emissions per kWh</p>
    <p>Conversion: 0.92 pounds / kWh × 454 = 417.305 gramm / kWh<br /> 417.305 gramms / kWh × 0.007 = 2.921135 gramms / 7 Wh</p>
    {(hiddenField #userId)}
    {submitButton { label = "Save changes" } }
|]
