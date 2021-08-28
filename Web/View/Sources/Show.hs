module Web.View.Sources.Show where

import Web.View.Prelude

data ShowView = ShowView {source :: Source}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={SourcesAction}>Sources</a></li>
                <li class="breadcrumb-item active">Show Source</li>
            </ol>
        </nav>
        <h1>Show Source</h1>
        <p>{source}</p>
    |]
