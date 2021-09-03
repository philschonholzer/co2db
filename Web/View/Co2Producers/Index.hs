{-# LANGUAGE OverloadedStrings #-}

module Web.View.Co2Producers.Index where

import Data.Text (Text, unpack, pack)
import Network.URL
import Web.View.Prelude
import Text.Printf
import Data.Fixed
import Application.Helper.Average



data IndexView = IndexView {co2Producers :: [Include "sources" Co2Producer], searchTerm :: Maybe Text}

instance View IndexView where
  html IndexView {..} =
    [hsx|
        <header>
          <h1>CO<sub>2</sub> Producers <a href={pathTo NewCo2ProducerAction} class="btn btn-primary ml-4">+ New</a></h1>
        </header>
        <form method="get">
          <div class="search-group">
            <input type="search" name="search" value={fromMaybe "" searchTerm} placeholder="Comma separate search terms to compare (e.g. 'beef, shower') " />
            <button type="submit">Search</button>
          </div>
        </form>
        <div class="producers">
            {forEach co2Producers renderCo2Producer}
        </div>
    |]


renderCo2Producer co2Producer =
  [hsx|
      <div class="producer index">
        <div class="title">
          <a href={ShowCo2ProducerAction (get #id co2Producer)}>
            <h2>
              {get #title co2Producer}
            </h2>
          </a>
        </div>
        <div class="fields">
          <div class="field co2-value">
            {renderCo2Value co2Producer $ get #sources co2Producer}
          </div>
          {editAndDeleteButtons} 
        </div>
      </div>
|]
  where
    renderCo2Value co2Producer sources = case average $ calcCo2PerUnit <$> sources of
        Just a -> [hsx|
            <p class="label">1{get #unit co2Producer}</p>
            <div class="amount-per-unit">
              <p style="font-size: 1.5em;">
                <span class="co2-amount">{a |> renderWeight}</span>&nbsp;CO<sub>2</sub>e
              </p>
            </div>
          |]
        Nothing -> [hsx|<p>-</p>|]
        where
          calcCo2PerUnit = (/) <$> get #gCo2e <*> get #per

    editAndDeleteButtons :: Html
    editAndDeleteButtons =
      case fromFrozenContext @(Maybe User) of
        Just user
          | get #id user == get #userId co2Producer ->
            [hsx|
                <div class="field">
                  <div class="edit-delete">
                    <a href={EditCo2ProducerAction (get #id co2Producer)}>Edit</a>&nbsp;
                    <a href={DeleteCo2ProducerAction (get #id co2Producer)} class="js-delete">Delete</a>
                  </div>
                </div>
              |]
        _ -> [hsx|  |]



--         <div class="field">
--           <p class="label">CO<sub>2</sub>e emissions</p>
--           <div class="amount-per-unit">
--             <span class="amount">{get #gCo2e co2Producer |> renderWeight}</span>
--             <span class="per fit">per <b>{get #per co2Producer |> renderPer}</b></span>
--             <span class="unit">{get #unit co2Producer}</span>
--           </div>
--         </div>
--         <div class="field">
--           <p class="label">Common CO<sub>2</sub>e consumption</p>
--           <div class="amount-per-unit">
--             <span class="amount">{calcAmountFromBase co2Producer commonConsumption}</span>
--             <span class="per fit">per <b>{get #commonConsumption co2Producer |> renderPer}</b></span>
--             <span class="unit">{get #unit co2Producer}</span>
--           </div>
--         </div>
--         <div class="field">
--           <p class="label">ø Yearly CO<sub>2</sub>e consumption</p>
--           <div class="amount-per-unit">
--             <span class="amount">{calcAmountFromBase co2Producer averageYearlyConsumption}</span>
--             <span class="per fit">per <b>{get #averageYearlyConsumption co2Producer |> renderPer}</b></span>
--             <span class="unit">{get #unit co2Producer}</span>
--           </div>
--         </div>