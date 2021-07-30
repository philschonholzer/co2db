module Web.View.Co2ProducerDetails.Show where

import Web.View.Prelude

data ShowView = ShowView {co2ProducerDetail :: Co2ProducerDetail}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={Co2ProducerDetailsAction}>Co2ProducerDetails</a></li>
                <li class="breadcrumb-item active">Show Co2ProducerDetail</li>
            </ol>
        </nav>
        <h1>Show Co2ProducerDetail</h1>
        <p>{co2ProducerDetail}</p>
    |]
