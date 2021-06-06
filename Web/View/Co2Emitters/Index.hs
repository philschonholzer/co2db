{-# LANGUAGE OverloadedStrings #-}

module Web.View.Co2Emitters.Index where

import Data.Text (Text, unpack, pack)
import Network.URL
import Web.View.Prelude
import Text.Printf
import Data.Fixed


data IndexView = IndexView {co2Emitters :: [Co2Emitter]}

instance View IndexView where
  html IndexView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={Co2EmittersAction}> Producers</a></li>
            </ol>
        </nav>
        <h1>CO<sub>2</sub> Producers <a href={pathTo NewCo2EmitterAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="producers">
            {forEach co2Emitters renderCo2Emitter}
        </div>
    |]
 --
renderCo2Emitter co2Emitter =
  [hsx|
      <div class="producer">
          <a href={ShowCo2EmitterAction (get #id co2Emitter)}>
            <h2>
              {get #title co2Emitter}
            </h2>
          </a>
          <div class="fields">
            <div class="field">
              <p>CO<sub>2</sub>e emissions</p>
              <div class="amount-per-unit">
                <span class="amount">{get #gCo2e co2Emitter |> renderWeight}</span>
                <span class="per fit">/</span>
                <span class="unit">{get #per co2Emitter |> renderPer} {get #unit co2Emitter}</span>
              </div>
            </div>
            <div class="field">
              <p>Common CO<sub>2</sub>e consumption</p>
              <div class="amount-per-unit">
                <span class="amount">{calcAmountFromBase co2Emitter commonConsumption}</span>
                <span class="per fit">/</span>
                <span class="unit">{get #commonConsumption co2Emitter |> renderPer} {get #unit co2Emitter}</span>
              </div>
            </div>
            <div class="field">
              <p>ø Yearly CO<sub>2</sub>e consumption</p>
              <div class="amount-per-unit">
                <span class="amount">{calcAmountFromBase co2Emitter averageYearlyConsumption}</span>
                <span class="per fit">/</span>
                <span class="unit">{get #averageYearlyConsumption co2Emitter |> renderPer} {get #unit co2Emitter}</span>
              </div>
            </div>
          </div>
          <div style="flex: 1 0 1em;"></div>
          <p class="source">{renderSource}</p>
          {editAndDeleteButtons}
      </div>
|]
  where
    renderSource =
      get #source co2Emitter
        |> unpack
        |> importURL
        |> ( \case
               Just url -> case url_type url of
                 Absolute url -> [hsx|<a href={get #source co2Emitter} target="_blank">{host url}</a>|]
                 _ -> noSource
               Nothing -> noSource
           )
    noSource = [hsx|<span class="muted">No source</span>|]

    editAndDeleteButtons :: Html
    editAndDeleteButtons =
      case fromFrozenContext @(Maybe User) of
        Just user
          | get #id user == get #userId co2Emitter |> fromMaybe "" ->
            [hsx|
                <div class="edit-delete"><a href={EditCo2EmitterAction (get #id co2Emitter)}>Edit</a>&nbsp;
                <a href={DeleteCo2EmitterAction (get #id co2Emitter)} class="js-delete">Delete</a></div>
              |]
        _ -> [hsx|  |]

    renderPer :: Double -> String
    renderPer amount 
              | amount `mod'` 1 == 0 = printf "%.f" amount
              | otherwise =  printf "%.2f" amount
