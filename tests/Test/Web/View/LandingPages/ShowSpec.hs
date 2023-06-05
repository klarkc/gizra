{-# LANGUAGE ImplicitParams #-}

module Test.Web.View.LandingPages.ShowSpec (tests) where

import Config (config)
import IHP.Controller.RequestContext (RequestBody (..), RequestContext (..))
import Network.Wai as Wai
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Test.Hspec.Expectations (shouldNotContain)
import Text.Blaze.Renderer.String as Blaze
import Web.Controller.Prelude (createRequestContext)
import Web.View.LandingPages.Show (ShowView (..))
import Web.View.Prelude
  ( Eq,
    Id',
    LandingPageWithRecords (..),
    ParagraphCta,
    ParagraphCta' (ParagraphCta, body),
    Record (newRecord),
    String,
    Text,
    buildFrameworkConfig,
    error,
    html,
    lift,
    newControllerContext,
    set,
    toHtml,
    undefined,
    (|>),
  )

tests = describe "Web.View.LandingPages.Show" do
  it "renders safe html" do
    let paragraphCta =
          newRecord @ParagraphCta
            |> set #body "<p>safe</p>"
        landingPageWithRecords =
          newRecord
            |> set #paragraphCtas [paragraphCta]
        view = ShowView {landingPageWithRecords}
    context <- createControllerContext
    let ?context = context
    let ?view = view
    let rendered = Blaze.renderMarkup (html view)
    rendered `shouldContain` "safe"
  it "sanitizes paragraph ctas html" do
    let paragraphCta =
          newRecord @ParagraphCta
            |> set #body "<script>unsafe</script>"
        landingPageWithRecords =
          newRecord
            |> set #paragraphCtas [paragraphCta]
        view = ShowView {landingPageWithRecords}
    context <- createControllerContext
    let ?context = context
    let ?view = view
    let rendered = Blaze.renderMarkup (html view)
    rendered `shouldNotContain` "unsafe"

createControllerContext = do
  frameworkConfig <- buildFrameworkConfig config
  let requestBody = FormBody {params = [], files = []}
      request = Wai.defaultRequest
      requestContext =
        RequestContext
          { request,
            respond = error "respond",
            requestBody,
            vault = error "vault",
            frameworkConfig
          }
  let ?requestContext = requestContext
  newControllerContext
