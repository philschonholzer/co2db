{-# LANGUAGE OverloadedStrings #-}

module Web.View.Co2Emitters.Index where

import Data.Text (Text, unpack, pack)
import Network.URL
import Web.View.Prelude
import Text.Printf
import Data.Fixed


data IndexView = IndexView {co2Emitters :: [Co2Emitter], searchTerm :: Maybe Text}

instance View IndexView where
  html IndexView {..} =
    [hsx|
        <header>
          <h1>CO<sub>2</sub> Producers <a href={pathTo NewCo2EmitterAction} class="btn btn-primary ml-4">+ New</a></h1>
        </header>
        <form method="get">
          <div class="search-group">
            <input type="search" name="search" value={fromMaybe "" searchTerm} placeholder="Comma separate search terms to compare (e.g. 'beef, shower') " />
            <button type="submit">Search</button>
          </div>
        </form>
        <div class="producers">
            {forEach co2Emitters renderCo2Emitter}
        </div>
    |]


renderCo2Emitter co2Emitter =
  [hsx|
      <div class="producer index">
        <div class="title">
          <a href={ShowCo2EmitterAction (get #id co2Emitter)}>
            <h2>
              {get #title co2Emitter}
            </h2>
          </a>
        </div>
        <div class="fields">
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
              <span class="per fit">per <b>{get #averageYearlyConsumption co2Emitter |> renderPer}</b></span>
              <span class="unit">{get #unit co2Emitter}</span>
            </div>
          </div>
          <div class="field">{editAndDeleteButtons}</div>
        </div>
      </div>
|]
  where

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

