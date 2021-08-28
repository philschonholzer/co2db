module Web.View.Sources.Index where

import Web.View.Prelude

data IndexView = IndexView {sources :: [Source]}

instance View IndexView where
  html IndexView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={SourcesAction}>Sources</a></li>
            </ol>
        </nav>
        <h1>Index</h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Source</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach sources renderSource}</tbody>
            </table>
        </div>
    |]

renderSource :: Source -> Html
renderSource source =
  [hsx|
    <tr>
        <td>{source}</td>
        <td><a href={ShowSourceAction (get #id source)}>Show</a></td>
        <td><a href={EditSourceAction (get #id source)} class="text-muted">Edit</a></td>
        <td><a href={DeleteSourceAction (get #id source)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
