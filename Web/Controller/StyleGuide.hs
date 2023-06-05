module Web.Controller.StyleGuide where

import Web.Controller.Prelude
import Web.View.StyleGuide.Show

instance Controller StyleGuideController where
  action ShowPersonAction = do
    let person =
          Person
            { name = "Jane Cooper",
              position = "Paradigm Representative",
              role = "Admin",
              email = "jane.cooper@acme.me",
              phone = "+1200-200-0000",
              avatar = "/avatar.jpg"
            }
    render ShowView {..}
