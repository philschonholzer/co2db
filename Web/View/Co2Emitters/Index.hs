{-# LANGUAGE OverloadedStrings #-}

module Web.View.Co2Emitters.Index where

import Data.Text (Text, unpack)
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
                <li class="breadcrumb-item active"><a href={Co2EmittersAction}> Emitters</a></li>
            </ol>
        </nav>
        <h1>Emitters <a href={pathTo NewCo2EmitterAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Name</th>
                        <th  colspan="3" class="text-center fit amount-of-per">CO<sub>2</sub>e emissions</th>
                        <th  colspan="3" class="text-center fit amount-of-per">Common CO<sub>2</sub>e consump.</th>
                        <th  colspan="3" class="text-center fit amount-of-per">ø Yearly CO<sub>2</sub>e consumption</th>
                        <th class="fit">Source</th>
                        <th class="text-right text-muted"></th>
                    </tr>
                </thead>
                <tbody>{forEach co2Emitters renderCo2Emitter}</tbody>
            </table>
        </div>
    |]

renderCo2Emitter co2Emitter =
  [hsx|
    <tr>
        <td class="fit">{get #title co2Emitter}</td>
        <td class="text-right fit amount-of-per">{get #gCo2e co2Emitter |> renderWeight}</td>
        <td class="per fit">/</td>
        <td class="text-muted fit per-of-amount">{get #per co2Emitter |> renderPer} {get #unit co2Emitter}</td>
        <td class="text-right fit amount-of-per">{calcAmountFromBase co2Emitter commonConsumption}</td>
        <td class="per fit">/</td>
        <td class="text-muted fit per-of-amount">{get #commonConsumption co2Emitter |> renderPer} {get #unit co2Emitter}</td>
        <td class="text-right fit amount-of-per">{calcAmountFromBase co2Emitter averageYearlyConsumption}</td>
        <td class="per fit">/</td>
        <td class="text-muted fit per-of-amount">{get #averageYearlyConsumption co2Emitter |> renderPer} {get #unit co2Emitter}</td>
        <td class="fit">{renderSource}</td>
        {editAndDeleteButtons}
    </tr>
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
    noSource = [hsx|<span class="text-muted">No source</span>|]

    editAndDeleteButtons :: Html
    editAndDeleteButtons =
      case fromFrozenContext @(Maybe User) of
        Just user
          | get #id user == get #userId co2Emitter |> fromMaybe "" ->
            [hsx|
                <td class="text-right text-muted"><a href={EditCo2EmitterAction (get #id co2Emitter)} class="text-muted">Edit</a>&nbsp;
                <a href={DeleteCo2EmitterAction (get #id co2Emitter)} class="js-delete text-muted">Delete</a></td>
              |]
        _ -> [hsx| <td></td> |]

    renderPer :: Double -> String
    renderPer amount 
              | amount `mod'` 1 == 0 = printf "%.f" amount
              | otherwise =  printf "%.2f" amount
