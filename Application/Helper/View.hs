module Application.Helper.View where

import IHP.ViewPrelude
import Text.HTML.SanitizeXSS (sanitize)

-- Here you can add functions which are available in all your views

sanitizeHtml :: Text -> Text
sanitizeHtml = sanitize
