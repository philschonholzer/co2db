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
    setOGUrl (urlTo (ShowCo2ProducerAction Nothing $ Just $ get #slug co2Producer ) )
    case get #description co2Producer of
      Just desc -> do
        setOGDescription desc
        setDescription desc
      Nothing -> pure ()
    case get #image co2Producer of
      Just url -> setOGImage url
      Nothing ->  setOGImage $ baseUrl getConfig <> "/meta-image.png"
  html ShowView {..} =
    [hsx|
        <article class="producer show">
          <header>
            <nav>
              <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={Co2ProducersAction}>Contributors</a></li>
                <li class="breadcrumb-item active">{get #title co2Producer}</li>
              </ol>
            </nav>
            <div class="title">
              <h1>{get #title co2Producer}<br><small>CO<sub>2</sub> emissions</small> </h1>
              {editAndDeleteButtons}
            </div>
          </header>
          <section class="co2-values">
            <div>
              <div class="co2-value">{renderCo2Value co2Producer $ get #sources co2Producer}</div>
              {renderDescription}
            </div>
            <div class="common-consumption">
              <h2>Possible footprints per person</h2>
              <div class="co2-value">
                {renderSingleConsumtion co2Producer}
              </div>
            </div>
          </section>
          <section>
            <div class="source_heading">
              <h2>Sources</h2>
              <a href={NewSourceAction (get #id co2Producer)}>Add Source</a>
            </div>
            <div class="details">
              {forEach (get #sources co2Producer) renderDetail}
            </div>
          </section>
        </article>
    |]
    where
      renderCo2Value co2Producer sources = case calcAverageCo2Value sources of
        Just a -> [hsx|
            <p style="font-size: 2em; margin: 0;">
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
              <svg style="height: 1em; width: 2em;" viewBox="-20 0 140 100">
                <path fill="transparent" stroke="black" stroke-width="10" d="M -10,50 L 90,50 M 50,10 L 90,50 L 50,90"/>
              </svg>
            |]

      renderDescription = case get #description co2Producer of
        Just a -> [hsx|
            <div class="description">
              <h2>Description</h2>
              <p>{a}</p>
            </div>
          |]
        Nothing -> [hsx||]

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
                  <div class="edit-delete"><a href={editLink}>Edit</a>&nbsp;&nbsp;
                  <a href={deleteLink} class="js-delete">Delete</a></div>
                |]
          _ -> [hsx|  |]
          where
            editLink = EditCo2ProducerAction (Just $ get #id co2Producer) Nothing
            deleteLink = DeleteCo2ProducerAction (Just $ get #id co2Producer) Nothing

      showFromString :: String -> Text
      showFromString = tshow

      renderDetail source = [hsx|
        <div class="detail">
          <div class="information">
            {get #name source}{renderInformation source}
          </div>
          <div class="fields data">
            <div class="field">
              <p class="label">CO<sub>2</sub>e for <b>{get #per source |> renderPer}</b> {get #unit co2Producer}</p>
              <div class="amount-per-unit">
                <span class="amount">{get #gCo2e source |> renderWeight}</span>
              </div>
            </div>
            <div class="field stretch">
              <p class="label"><b>Source</b></p>
              <div class="source">
                <p class="">{renderMarkdown $ get #description source}</p>
              </div>
            </div>
            {editAndDeleteDetailButtons source}
          </div>
        </div>
        |]

      renderInformation source = [hsx|{renderField (get #region source)}{renderField (get #year source)}|]
        where
          renderField (Just field) = ", " <> show field
          renderField Nothing = ""

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

      renderSingleConsumtion co2Producer = case calcAverageCo2Value $ get #sources co2Producer of
        Just agCo2 -> [hsx|
            {commonConsumption co2Producer agCo2}
          |]
        Nothing -> [hsx|<p>-</p>|]
        where
          commonConsumption co2Producer agCo2 = componentFromState
            CommonConsumption {
              amount    = get #singleConsumptionAverage co2Producer,
              minAmount = get #singleConsumptionFrom co2Producer,
              maxAmount = get #singleConsumptionTo co2Producer,
              timesPerYear = get #timesPerYearAverage co2Producer,
              minTimesPerYear = get #timesPerYearFrom co2Producer,
              maxTimesPerYear = get #timesPerYearTo co2Producer,
              gCo2      = agCo2,
              unit      = get #unit co2Producer
            }


