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
          <h1>CO<sub>2</sub> Contributors <a href={pathTo NewCo2ProducerAction} class="btn btn-primary ml-4">+ New</a></h1>
        </header>
        <form method="get" action="/">
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
          <a href={getLink}>
            <h2>
              {get #title co2Producer}
            </h2>
          </a>
        </div>
        <div class="fields">
          {renderCo2Value co2Producer $ get #sources co2Producer}
          {editAndDeleteButtons} 
        </div>
      </div>
|]
  where
    getLink = ShowCo2ProducerAction { co2ProducerId = Nothing, slug = Just $ get #slug co2Producer  }
    renderCo2Value co2Producer sources = case calcAverageCo2Value sources of
      Just a -> [hsx|
          <div class="field">
            <p class="label">Possible CO<sub>2</sub>e per person-year</p>
            <div class="amount-per-unit">
              <p style="font-size: 1.5em;">
                <span class="co2-amount">{calcPossibleCo2PerYear co2Producer a |> renderWeight}</span>
              </p>
            </div>
          </div>
          <div class="field" style="display: flex; justify-content: space-between; align-items: baseline;">
            <p class="label">1 {get #unit co2Producer}</p>
            <p>{a |> (/1000) |> formatDecimal}&nbsp;kg&nbsp;CO<sub>2</sub>e</p>
          </div>
        |]
      Nothing -> [hsx|<p>-</p>|]

    editAndDeleteButtons :: Html
    editAndDeleteButtons =
      case fromFrozenContext @(Maybe User) of
        Just user
          | get #id user == get #userId co2Producer ->
            [hsx|
                <div class="field">
                  <div class="edit-delete">
                    <a href={EditCo2ProducerAction (Just $ get #id co2Producer) Nothing}>Edit</a>&nbsp;
                    <a href={DeleteCo2ProducerAction (Just $ get #id co2Producer) Nothing} class="js-delete">Delete</a>
                  </div>
                </div>
              |]
        _ -> [hsx|  |]
