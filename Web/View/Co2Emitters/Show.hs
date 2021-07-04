module Web.View.Co2Emitters.Show where

import qualified Text.MMark as MMark
import qualified Text.Megaparsec   as M
import Web.View.Prelude
import Text.Printf

data ShowView = ShowView {co2Emitter :: Co2Emitter}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <article class="producer show">
          <header>
            <nav>
              <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={Co2EmittersAction}>Producers</a></li>
                <li class="breadcrumb-item active">{get #title co2Emitter}</li>
              </ol>
            </nav>
            <div class="title">
              <h1>CO<sub>2</sub> Footprint of {get #title co2Emitter}</h1>
              {editAndDeleteButtons}
            </div>
          </header>
          <div class="section-layout">
            <section class="fields data">
              <div class="field">
                <p class="label">CO<sub>2</sub>e emissions</p>
                <div class="amount-per-unit">
                  <span class="amount">{get #gCo2e co2Emitter |> renderWeight}</span>
                  <span class="per fit">per <b>{get #per co2Emitter |> renderPer}</b></span>
                  <span class="unit">{get #unit co2Emitter}</span>
                </div>
              </div>
              <div class="field">
                <p class="label">Common CO<sub>2</sub>e consumption</p>
                <div class="amount-per-unit">
                  <span class="amount">{calcAmountFromBase co2Emitter commonConsumption}</span>
                  <span class="per fit">per <b>{get #commonConsumption co2Emitter |> renderPer}</b></span>
                  <span class="unit">{get #unit co2Emitter}</span>
                </div>
              </div>
              <div class="field">
                <p class="label">ø Yearly CO<sub>2</sub>e consumption</p>
                <div class="amount-per-unit">
                  <span class="amount">{calcAmountFromBase co2Emitter averageYearlyConsumption}</span>
                  <span class="per fit">per <b>{get #averageYearlyConsumption co2Emitter |> renderPer}</b> </span>
                  <span class="unit">{get #unit co2Emitter}</span>
                </div>
              </div>
            </section>
            <section class="description"><h2>Description</h2>{renderDescription}</section>
            <section class="source"><h2>Source</h2>{get #source co2Emitter |> renderMarkdown}</section>
          </div>
          
        </article>
    |]
    where
      renderDescription = case get #description co2Emitter of
        Just a -> [hsx|<p>{a}</p>|]
        Nothing -> [hsx|<p class="muted">No description</p>|]

      renderMarkdown text =
        case text |> MMark.parse "" of
          Left error -> M.errorBundlePretty error |> printf "Error: %s" |> showFromString |> preEscapedToHtml
          Right markdown -> MMark.render markdown |> tshow |> preEscapedToHtml

      editAndDeleteButtons :: Html
      editAndDeleteButtons =
        case fromFrozenContext @(Maybe User) of
          Just user
            | get #id user == get #userId co2Emitter |> fromMaybe "" ->
              [hsx|
                  <div class="edit-delete"><a href={EditCo2EmitterAction (get #id co2Emitter)}>Edit</a>&nbsp;&nbsp;
                  <a href={DeleteCo2EmitterAction (get #id co2Emitter)} class="js-delete">Delete</a></div>
                |]
          _ -> [hsx|  |]

      showFromString :: String -> Text 
      showFromString = tshow