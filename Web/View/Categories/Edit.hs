module Web.View.Categories.Edit where
import Web.View.Prelude

data EditView = EditView { category :: Category }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CategoriesAction}>Categories</a></li>
                <li class="breadcrumb-item active">Edit Category</li>
            </ol>
        </nav>
        <h1>Edit Category</h1>
        {renderForm category}
    |]

renderForm :: Category -> Html
renderForm category = formFor category [hsx|
    {(textField #title)}
    {submitButton}
|]
