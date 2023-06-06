module Web.Controller.StyleGuide where

import Web.Controller.Prelude
import Web.View.StyleGuide.Index
import Web.View.StyleGuide.Show

examplePerson :: Person
examplePerson =
  Person
    { name = "Jane Cooper",
      position = "Paradigm Representative",
      role = "Admin",
      email = "jane.cooper@acme.me",
      phone = "+1200-200-0000",
      avatar = "/avatar.jpg"
    }

instance Controller StyleGuideController where
  action StyleGuideAction =
    let persons = replicate 10 examplePerson
     in render IndexView {..}
  action ShowPersonAction =
    let person = examplePerson
     in render ShowView {..}
