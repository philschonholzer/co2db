module Web.View.Categories.New where

import Web.View.Prelude

data NewView = NewView {category :: Category}

instance View NewView where
  html NewView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CategoriesAction}>Categories</a></li>
                <li class="breadcrumb-item active">New Category</li>
            </ol>
        </nav>
        <h1>New Category</h1>
        {renderForm category}
    |]

renderForm :: Category -> Html
renderForm category =
  formFor
    category
    [hsx|
    {(textField #title)}
    {submitButton}
|]
