module Web.View.StyleGuide.Show where

import Web.Element.ElementBuild
import Web.Element.ElementWrap
import Web.Element.Types
import Web.Types
import Web.View.Prelude

newtype ShowView = ShowView {person :: Person}

instance View ShowView where
  html ShowView {..} =
    [hsx|
        {header}
        <div class="max-w-sm w-full mx-auto">
          {renderPerson person }
        </div>

    |]
    where
      breadcrumb =
        renderBreadcrumb
          [ breadcrumbLink "StyleGuide" StyleGuideAction,
            breadcrumbText "Show Person"
          ]

      header =
        [hsx|
                    {breadcrumb}
                    {name}
                |]
          |> wrapVerticalSpacing AlignNone
          |> wrapContainerWide

      name =
        cs person.name
          |> wrapHeaderTag 1
          |> wrapHorizontalSpacingTiny AlignBaseline

renderPerson :: (?context :: ControllerContext) => Person -> Html
renderPerson person =
  [hsx|
    <div class="rounded-lg shadow">
        <div class="flex flex-col items-center space-y-8">
            <div class="rounded-full overflow-hidden mt-8 mx-8" style="max-width: 128px; max-height: 128px;">
                <img src={person.avatar} alt={person.name}>
            </div> 
            <div class="text-center space-y-2 text-gray-900">
              <div class="text-sm leading-5 font-medium mx-8">
                  {person.name}
              </div>
              <div class="text-gray-500 text-sm leading-5 font-normal mx-8">
                  {person.position}
              </div>
              <div class="mx-8">
                  <div class="inline-block text-green-800 text-xs leading-4 font-medium bg-green-100 rounded-full px-2 py-1">{person.role}</div>
              </div>
            </div>
        </div>
        <div class="grid grid-cols-2 text-gray-700 divide-x divide-solid divide-gray-200 mt-8 border border-solid border-gray-200">
            <a class="flex place-content-center text-center p-4" href={"mailto:" <> person.email} target="_blank">
              <img src="/email.svg" class="mr-2">
              Email
            </a>
            <a class="flex place-content-center text-center p-4" href={"tel:" <> person.phone} target="_blank">
              <img src="/phone.svg" class="mr-2">
              Call
            </a>
        </div>
    </div>
    |]
