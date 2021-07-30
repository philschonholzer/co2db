module Web.View.Co2ProducerDetails.Edit where

import Web.View.Prelude

data EditView = EditView {co2ProducerDetail :: Co2ProducerDetail}

instance View EditView where
  html EditView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={Co2ProducerDetailsAction}>Co2ProducerDetails</a></li>
                <li class="breadcrumb-item active">Edit Co2ProducerDetail</li>
            </ol>
        </nav>
        <h1>Edit Co2ProducerDetail</h1>
        {renderForm co2ProducerDetail}
    |]

renderForm :: Co2ProducerDetail -> Html
renderForm co2ProducerDetail =
  formFor
    co2ProducerDetail
    [hsx|
    {(textField #co2ProducerId)}
    {(textField #region)}
    {(textField #year)}
    {(textField #gCo2e)}
    {(textField #commonConsumption)}
    {(textField #averageYearlyConsumption)}
    {(textField #per)}
    {(textField #unit)}
    {(textField #source)}
    {(textField #userId)}
    {submitButton}
|]
