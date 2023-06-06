module Web.View.StyleGuide.Index where

import Web.Element.ElementBuild
import Web.Element.ElementWrap
import Web.Element.Types
import Web.View.Prelude
import Web.View.StyleGuide.Show (renderPerson)

newtype IndexView = IndexView {persons :: [Person]}

instance View IndexView where
  html IndexView {..} =
    [hsx|
        {breadcrumb}

        {header}
        {body}

    |]
      |> wrapVerticalSpacing AlignNone
      |> wrapContainerWide
    where
      header =
        [ [hsx|StyleGuide Index|] |> wrapHeaderTag 1
        ]
          |> mconcat
          |> wrapHorizontalSpacing AlignNone

      body =
        [hsx|
          <div class="grid gap-8 md:grid-cols-2 lg:grid-cols-3">
            {forEach persons renderPerson}
          </div>
        |]

      breadcrumb =
        renderBreadcrumb
          [ breadcrumbLink "StyleGuide" StyleGuideAction
          ]