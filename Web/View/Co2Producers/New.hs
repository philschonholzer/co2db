module Web.View.Co2Producers.New where

import Web.View.Prelude

data NewView = NewView {co2Producer :: Co2Producer, categories :: [Category]}

instance View NewView where
  html NewView {..} =
    [hsx|
        <header>
          <nav>
            <ol class="breadcrumb">
              <li class="breadcrumb-item"><a href={Co2ProducersAction}>Contributors</a></li>
              <li class="breadcrumb-item active">New {get #title co2Producer}</li>
            </ol>
          </nav>

          <h1>New CO<sub>2</sub> Contributor</h1>
        </header>
        <section>{renderForm co2Producer categories}</section>
    |]

renderForm :: Co2Producer -> [Category] -> Html
renderForm co2Producer categories =
  formFor
    co2Producer
    [hsx|
      {(textField #title) { required = True }}
      {(textField #slug)  { disabled = True }}
      {(textField #description)}
      {(selectField #categoryId categories) { required = True }}
      {(selectField #unit $ allEnumValues @Unit ) { required = True }}
    
      <fieldset>
        <legend>Single common consumption</legend>
        <div style="display: flex; gap: 1em;">
          {(textField #singleConsumptionFrom) { required = True, fieldLabel = "From", helpText = commonHelpText }}
          {(textField #singleConsumptionAverage) { required = True, fieldLabel = "Average", helpText = commonHelpText }}
          {(textField #singleConsumptionTo) { required = True, fieldLabel = "To", helpText = commonHelpText }}
        </div>
        <p><small>What is a common single unit that is consumed of this producer.</small></p>
      </fieldset>
      <fieldset>
        <legend>Times per year consumed</legend>
        <div style="display: flex; gap: 1em;">
          {(textField #timesPerYearFrom) { required = True, fieldLabel = "From" }}
          {(textField #timesPerYearAverage) { required = True, fieldLabel = "Average" }}
          {(textField #timesPerYearTo) { required = True, fieldLabel = "To" }}
        </div>
        <p><small>How many times is a single unit consumed per year.</small></p>
      </fieldset>
      {(textField #image)}
      {submitButton { label = "Next" } }
    |]
  where
    commonHelpText = "in Units"
