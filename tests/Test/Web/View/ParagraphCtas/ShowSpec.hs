{-# LANGUAGE ImplicitParams #-}

module Test.Web.View.ParagraphCtas.ShowSpec (tests) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Blaze.Renderer.String as Blaze
import Web.View.ParagraphCtas.Show (ShowView (..))
import Web.View.Prelude
  ( Eq,
    Id',
    ParagraphCta,
    ParagraphCta' (ParagraphCta, body),
    Record (newRecord),
    String,
    Text,
    html,
    lift,
    newControllerContext,
    set,
    toHtml,
    undefined,
    (|>),
  )

tests = describe "Web.View.ParagraphCtas.Show" do
  it "sanitizes html" do
    let landingPageId = "foo"
        weight = 1
        paragraphCta =
          newRecord
            |> set #landingPageId landingPageId
            |> set #weight weight
        view = ShowView {paragraphCta = paragraphCta}
    let ?view = view
        ?requestContext = undefined
    context <- newControllerContext
    let ?context = context
    html view `shouldRenderTo` "unsafe"

shouldRenderTo renderFn expHtml =
  Blaze.renderMarkup renderFn `shouldBe` expHtml
