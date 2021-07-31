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
        {(textField #image)}
        {submitButton}
    |]
