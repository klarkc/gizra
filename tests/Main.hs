module Main where

import IHP.Prelude (IO)
import Test.Hspec (Spec, hspec)
import Test.Web.View.LandingPages.ShowSpec as LandingPages.ShowSpec

main :: IO ()
main = hspec do
  LandingPages.ShowSpec.tests
