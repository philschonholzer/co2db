module Web.View.Co2Producers.Show where

import qualified Text.MMark as MMark
import qualified Text.Megaparsec   as M
import qualified Text.Blaze.Html5 as H

import Web.View.Prelude
import Text.Printf

data ShowView = ShowView {co2Producer :: Include "co2ProducerDetails" Co2Producer}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <article class="producer show">
          <header>
            <nav>
              <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={Co2ProducersAction}>Producers</a></li>
                <li class="breadcrumb-item active">{get #title co2Producer}</li>
              </ol>
            </nav>
            <div class="title">
              <h1>CO<sub>2</sub> Footprint of {get #title co2Producer}</h1>
              {editAndDeleteButtons}
            </div>
          </header>
          <section class="description"><h2>Description</h2>{renderDescription}</section>
          <a href={NewCo2ProducerDetailAction (get #id co2Producer)}>Add Source</a>
          <div class="details">
            {forEach (get #co2ProducerDetails co2Producer) renderDetail}
          </div>
          
        </article>
    |]
    where
      renderDescription = case get #description co2Producer of
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
            | get #id user == get #userId co2Producer |> fromMaybe "" ->
              [hsx|
                  <div class="edit-delete"><a href={EditCo2ProducerAction (get #id co2Producer)}>Edit</a>&nbsp;&nbsp;
                  <a href={DeleteCo2ProducerAction (get #id co2Producer)} class="js-delete">Delete</a></div>
                |]
          _ -> [hsx|  |]

      showFromString :: String -> Text 
      showFromString = tshow

      renderDetail co2ProducerDetail = [hsx|
        <div class="detail">
          <div class="information">
            {renderInformation co2ProducerDetail}
          </div>
          <div class="fields data">
            <div class="field">
              <p class="label">CO<sub>2</sub>e emissions</p>
              <div class="amount-per-unit">
                <span class="amount">{get #gCo2e co2ProducerDetail |> renderWeight}</span>
                <span class="per fit">per <b>{get #per co2ProducerDetail |> renderPer}</b></span>
                <span class="unit">{get #unit co2ProducerDetail}</span>
              </div>
            </div>
            <div class="field">
              <p class="label">Common CO<sub>2</sub>e consumption</p>
              <div class="amount-per-unit">
                <span class="amount">{calcAmountFromBaseDetail co2ProducerDetail commonConsumption}</span>
                <span class="per fit">per <b>{get #commonConsumption co2ProducerDetail |> renderPer}</b></span>
                <span class="unit">{get #unit co2ProducerDetail}</span>
              </div>
            </div>
            <div class="field">
              <p class="label">ø Yearly CO<sub>2</sub>e consumption</p>
              <div class="amount-per-unit">
                <span class="amount">{calcAmountFromBaseDetail co2ProducerDetail averageYearlyConsumption}</span>
                <span class="per fit">per <b>{get #averageYearlyConsumption co2ProducerDetail |> renderPer}</b> </span>
                <span class="unit">{get #unit co2ProducerDetail}</span>
              </div>
            </div>
            <div class="field">
              <p class="label">Source</p>
              <div class="source">
                <p class="">{renderMarkdown $ get #source co2ProducerDetail}</p>
              </div>
            </div>
            {editAndDeleteDetailButtons co2ProducerDetail}
          </div>
        </div>
        |]

      renderInformation co2ProducerDetail = if (isJust $ get #year co2ProducerDetail) || (isJust $ get #region co2ProducerDetail)
                                            then [hsx|{fromMaybe "" $ get #region co2ProducerDetail}{renderYear $ get #year co2ProducerDetail}|]
                                            else [hsx|<span class="muted">Not specified</span>|]
        where
          renderYear (Just year) = ", " <> show year
          renderYear Nothing = "" 
  
      calcAmountFromBaseDetail :: (?context :: ControllerContext) => Co2ProducerDetail' co2ProducerId userId co2Producers -> (Co2ProducerDetail' co2ProducerId userId co2Producers  -> Double) -> H.Html
      calcAmountFromBaseDetail co2Producer consumption =
        co2Producer
          |> get #gCo2e
          |> (/ get #per co2Producer)
          |> (* consumption co2Producer)
          |> renderWeight

      editAndDeleteDetailButtons :: Co2ProducerDetail -> Html
      editAndDeleteDetailButtons co2ProducerDetail =
        case fromFrozenContext @(Maybe User) of
          Just user
            | get #id user == get #userId co2ProducerDetail |> fromMaybe "" ->
              [hsx|
                  <div class="field">
                    <div class="edit-delete"><a href={EditCo2ProducerDetailAction (get #id co2ProducerDetail)}>Edit</a>&nbsp;&nbsp;
                    <a href={DeleteCo2ProducerDetailAction (get #id co2ProducerDetail)} class="js-delete">Delete</a></div>
                  </div>
                |]
          _ -> [hsx|  |]

