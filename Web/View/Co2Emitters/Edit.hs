module Web.View.Co2Emitters.Edit where

import Web.View.Prelude

data EditView = EditView {co2Emitter :: Co2Emitter, categories :: [Category]}

instance View EditView where
  html EditView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={Co2EmittersAction}>Co2Emitters</a></li>
                <li class="breadcrumb-item active">Edit Co2Emitter</li>
            </ol>
        </nav>
        <h1>Edit Co2Emitter</h1>
        {renderForm co2Emitter categories}
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
        {(textField #source)}
        {(textField #image)}
        {submitButton}
    |]
