module Web.View.Categories.Show where
import Web.View.Prelude

data ShowView = ShowView { category :: Category }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CategoriesAction}>Categories</a></li>
                <li class="breadcrumb-item active">Show Category</li>
            </ol>
        </nav>
        <h1>Show Category</h1>
        <p>{category}</p>
    |]
