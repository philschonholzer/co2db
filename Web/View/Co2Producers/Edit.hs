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
      {(textField #title) { required = True }}
      {(textField #description)}
      {(selectField #categoryId categories) { required = True }}
      {(selectField #unit $ allEnumValues @Unit ) { required = True }}
    
      <fieldset>
        <legend>Single common consumption</legend>
        <div style="display: flex; gap: 1em;">
          {(textField #commonSingleConsumptionFrom) { required = True, fieldLabel = "From", helpText = commonHelpText }}
          {(textField #commonSingleConsumptionTo) { required = True, fieldLabel = "To", helpText = commonHelpText }}
          {(textField #commonSingleConsumptionAverage) { required = True, fieldLabel = "Average", helpText = commonHelpText }}
        </div>
        <p><small>What is a common <b>single</b> unit to consume of this producer?</small></p>
      </fieldset>
      <fieldset>
        <legend>Yearly common consumption</legend>
        <div style="display: flex; gap: 1em;">
          {(textField #commonYearlyConsumptionFrom) { required = True, fieldLabel = "From", helpText = commonHelpText }}
          {(textField #commonYearlyConsumptionTo) { required = True, fieldLabel = "To", helpText = commonHelpText }}
          {(textField #commonYearlyConsumptionAverage) { required = True, fieldLabel = "Average", helpText = commonHelpText }}
        </div>
        <p><small>What is a common <b>yearly</b> unit to consume of this producer?</small></p>
      </fieldset>
      {(textField #image)}
      {submitButton { label = "Add CO2 Producer" } }
    |]
  where
    commonHelpText = "in Units"
