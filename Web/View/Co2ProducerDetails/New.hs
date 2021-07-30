module Web.View.Co2ProducerDetails.New where

import Web.View.Prelude

data NewView = NewView {co2ProducerDetail :: Co2ProducerDetail, co2Producer :: Co2Producer}

instance View NewView where
  html NewView {..} =
    [hsx|
      <header>
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={Co2ProducersAction}>Producers</a></li>
                <li class="breadcrumb-item"><a href={ShowCo2ProducerAction (get #id co2Producer)}>{get #title co2Producer}</a></li>
                <li class="breadcrumb-item active">Add Source</li>
            </ol>
        </nav>

        <h1>New Co2ProducerDetail for <q>{get #title co2Producer }</q></h1>
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
    {(textField #gCo2e) { required = True, fieldLabel = "Grams (g) CO₂e"}}
    {(textField #commonConsumption) { required = True }}
    {(textField #averageYearlyConsumption) { required = True }}
    {(textField #per) { required = True }}
    {(selectField #unit $ enumFrom Kilometer ) { required = True }}
    {(textField #source) { required = True }}
    {submitButton}
|]
