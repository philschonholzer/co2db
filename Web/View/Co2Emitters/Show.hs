module Web.View.Co2Emitters.Show where

import Web.View.Prelude

data ShowView = ShowView {co2Emitter :: Co2Emitter}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={Co2EmittersAction}>Emitters</a></li>
                <li class="breadcrumb-item active">Emissions of {get #title co2Emitter}</li>
            </ol>
        </nav>
        <h1>Emissions of {get #title co2Emitter}</h1>
        {renderDescription}
        <p>{get #gCo2e co2Emitter |> renderWeight} / {get #per co2Emitter}{get #unit co2Emitter}</p>
    |]
    where
      renderDescription = case get #description co2Emitter of
        Just a -> [hsx|<p>{a}</p>|]
        Nothing -> [hsx||]
