module Web.View.Categories.Index where
import Web.View.Prelude

data IndexView = IndexView { categories :: [Category] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={CategoriesAction}>Categories</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewCategoryAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Category</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach categories renderCategory}</tbody>
            </table>
        </div>
    |]


renderCategory category = [hsx|
    <tr>
        <td>{category}</td>
        <td><a href={ShowCategoryAction (get #id category)}>Show</a></td>
        <td><a href={EditCategoryAction (get #id category)} class="text-muted">Edit</a></td>
        <td><a href={DeleteCategoryAction (get #id category)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
