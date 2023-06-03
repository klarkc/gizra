module Main where

import IHP.Prelude (IO)
import Test.Hspec (Spec, hspec)
import Test.Web.View.ParagraphCtas.ShowSpec as ShowSpec

main :: IO ()
main = hspec do
  ShowSpec.tests
