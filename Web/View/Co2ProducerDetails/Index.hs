module Web.View.Co2ProducerDetails.Index where

import Web.View.Prelude

data IndexView = IndexView {co2ProducerDetails :: [Co2ProducerDetail]}

instance View IndexView where
  html IndexView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={Co2ProducerDetailsAction}>Co2ProducerDetails</a></li>
            </ol>
        </nav>
        <h1>Index</h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Co2ProducerDetail</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach co2ProducerDetails renderCo2ProducerDetail}</tbody>
            </table>
        </div>
    |]

renderCo2ProducerDetail :: Co2ProducerDetail -> Html
renderCo2ProducerDetail co2ProducerDetail =
  [hsx|
    <tr>
        <td>{co2ProducerDetail}</td>
        <td><a href={ShowCo2ProducerDetailAction (get #id co2ProducerDetail)}>Show</a></td>
        <td><a href={EditCo2ProducerDetailAction (get #id co2ProducerDetail)} class="text-muted">Edit</a></td>
        <td><a href={DeleteCo2ProducerDetailAction (get #id co2ProducerDetail)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
