{-# LANGUAGE OverloadedStrings #-}

module Web.View.Co2Emitters.Index where

import Data.Text (Text, unpack)
import Network.URL
import Web.View.Prelude

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
                        <th>Amount</th>
                        <th class="text-right fit">CO<sub>2</sub> emissions</th>
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
        <td class="text-muted fit">{get #per co2Emitter} {get #unit co2Emitter}</td>
        <td class="text-right fit">{get #gCo2e co2Emitter |> renderWeight}</td>
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
