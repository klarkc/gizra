module Web.View.Prelude
( module IHP.ViewPrelude
, module Web.View.Layout
, module Generated.Types
, module Web.Types
, module Application.Helper.View
, sanitizeHtml
) where

import IHP.ViewPrelude
import Web.View.Layout
import Generated.Types
import Web.Types
import Web.Routes ()
import Application.Helper.View
import Text.HTML.SanitizeXSS (sanitize)

sanitizeHtml :: Text -> Text
sanitizeHtml = sanitize
