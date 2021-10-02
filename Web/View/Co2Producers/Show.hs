module Web.View.Co2Producers.Show where

import Numeric
import qualified Text.MMark as MMark
import qualified Text.Megaparsec   as M
import qualified Text.Blaze.Html5 as H

import Web.View.Prelude
import Text.Printf
import Application.Helper.Average
import Control.Applicative
import IHP.ServerSideComponent.ViewFunctions
import Web.Component.CommonConsumption
import Web.Controller.Prelude (setOGImage)

data ShowView = ShowView {co2Producer :: Include "sources" Co2Producer}

instance View ShowView where
  beforeRender ShowView { co2Producer } = do
    setOGTitle (get #title co2Producer)
    setOGUrl (urlTo (ShowCo2ProducerAction (get #id co2Producer)) )
    case get #description co2Producer of 
      Just desc -> do 
        setOGDescription desc
        setDescription desc
      Nothing -> pure ()
    case get #image co2Producer of
      Just url -> setOGImage url
      Nothing -> setOGImage "/logo.png"
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
          <section class="co2-values">
            <div class="co2-value">{renderCo2Value co2Producer $ get #sources co2Producer}</div>
            <div class="common-consumption">
              <h2>Common consumptions</h2>
              <div class="co2-value">
                <h3>Single consumption</h3>
                {renderSingleConsumtion co2Producer}
              </div>
              <div class="co2-value">
                <h3>Yearly consumption</h3>
                {renderYearlyConsumtion co2Producer}
              </div>
            </div>
          </section>
          <section class="description"><h2>Description</h2>{renderDescription}</section>
          <a href={NewSourceAction (get #id co2Producer)}>Add Source</a>
          <div class="details">
            {forEach (get #sources co2Producer) renderDetail}
          </div>
        </article>
    |]
    where
      renderCo2Value co2Producer sources = case averageCo2Value sources of
        Just a -> [hsx|
            <p style="font-size: 2em;">
              <span class="producer-amount">1</span>
              <span class="producer-unit">{get #unit co2Producer}</span>
              {svg}
              <span class="co2-amount">{a |> renderWeight}</span>&nbsp;CO<sub>2</sub>e
            </p>
          |]
        Nothing -> [hsx|<p>-</p>|]
        where
          svg = 
            [hsx|
              <svg style="height: 1em; width: 3em;" viewBox="-20 0 140 100">
                <path fill="transparent" stroke="black" stroke-width="10" d="M -10,50 L 90,50 M 50,10 L 90,50 L 50,90"/>
              </svg>
            |]

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
            | get #id user == get #userId co2Producer ->
              [hsx|
                  <div class="edit-delete"><a href={EditCo2ProducerAction (get #id co2Producer)}>Edit</a>&nbsp;&nbsp;
                  <a href={DeleteCo2ProducerAction (get #id co2Producer)} class="js-delete">Delete</a></div>
                |]
          _ -> [hsx|  |]

      showFromString :: String -> Text
      showFromString = tshow

      renderDetail source = [hsx|
        <div class="detail">
          <div class="information">
            {renderInformation source}
          </div>
          <div class="fields data">
            <div class="field">
              <p class="label">CO<sub>2</sub>e for <b>{get #per source |> renderPer}</b> {get #unit co2Producer}</p>
              <div class="amount-per-unit">
                <span class="amount">{get #gCo2e source |> renderWeight}</span>
              </div>
            </div>
            <div class="field stretch">
              <p class="label">Source</p>
              <div class="source">
                <p class="">{renderMarkdown $ get #description source}</p>
              </div>
            </div>
            {editAndDeleteDetailButtons source}
          </div>
        </div>
        |]

      renderInformation source = if isJust (get #year source) || isJust (get #region source)
                                            then [hsx|{fromMaybe "" $ get #region source}{renderYear $ get #year source}|]
                                            else [hsx|<span class="muted">Not specified</span>|]
        where
          renderYear (Just year) = ", " <> show year
          renderYear Nothing = ""

      editAndDeleteDetailButtons :: Source -> Html
      editAndDeleteDetailButtons source =
        case fromFrozenContext @(Maybe User) of
          Just user
            | get #id user == get #userId source ->
              [hsx|
                  <div class="field">
                    <div class="edit-delete"><a href={EditSourceAction (get #id source)}>Edit</a>&nbsp;&nbsp;
                    <a href={DeleteSourceAction (get #id source)} class="js-delete">Delete</a></div>
                  </div>
                |]
          _ -> [hsx|  |]

      renderSingleConsumtion co2Producer = case averageCo2Value $ get #sources co2Producer of
        Just agCo2 -> [hsx|
            {commonConsumption co2Producer agCo2}
          |]
        Nothing -> [hsx|<p>-</p>|]
        where
          commonConsumption co2Producer agCo2 = componentFromState 
            CommonConsumption { 
              amount    = get #commonSingleConsumptionAverage co2Producer, 
              minAmount = get #commonSingleConsumptionFrom co2Producer, 
              maxAmount = get #commonSingleConsumptionTo co2Producer,
              gCo2      = agCo2,
              unit      = get #unit co2Producer
            }

      renderYearlyConsumtion co2Producer = case averageCo2Value $ get #sources co2Producer of
        Just agCo2 -> [hsx|
            {commonConsumption co2Producer agCo2}
          |]
        Nothing -> [hsx|<p>-</p>|]
        where
          commonConsumption co2Producer agCo2 = componentFromState 
            CommonConsumption { 
              amount    = get #commonYearlyConsumptionAverage co2Producer, 
              minAmount = get #commonYearlyConsumptionFrom co2Producer, 
              maxAmount = get #commonYearlyConsumptionTo co2Producer,
              gCo2      = agCo2,
              unit      = get #unit co2Producer
            }

