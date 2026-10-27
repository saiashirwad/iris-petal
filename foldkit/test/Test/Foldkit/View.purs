module Test.Foldkit.View (spec) where

import Prelude

import Control.Monad.State.Class (modify_)
import Foldkit (Program)
import Foldkit.Html
  ( Role(..)
  , ariaDescribedBy
  , ariaLabel
  , article
  , button
  , div
  , form
  , h2
  , id_
  , input
  , label
  , onInput
  , onSubmit
  , p
  , role_
  , text
  , title_
  , type_
  , value_
  , for_
  )
import Foldkit.Locator (named, role, within)
import Foldkit.Locator as Locator
import Foldkit.Render (renderToString)
import Foldkit.Test
  (Check(..), expect, expectAbsent, expectCount, expectModel, expectPresent, fill, submit)
import Foldkit.Test as Test
import Test.Foldkit.Scenario (failsWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Message = Typed String | Submitted

type Model = { draft :: String, saved :: Array String }

program :: Program Model Message
program =
  { model: { draft: "", saved: [] }
  , init: pure unit
  , update: case _ of
      Typed draft -> modify_ _ { draft = draft }
      Submitted -> modify_ \model -> model { saved = model.saved <> [ model.draft ], draft = "" }
  , view: \model -> div []
      [ form [ onSubmit Submitted, ariaLabel "Add note" ]
          [ label [ for_ "note" ] [ text "Note" ]
          , input [ id_ "note", value_ model.draft, onInput Typed, ariaDescribedBy "note-help" ]
          , p [ id_ "note-help" ] [ text "Short and sweet." ]
          , button [ type_ "submit" ] [ text "Save" ]
          ]
      , div [ role_ Status ] (map (\note -> article [] [ h2 [] [ text note ] ]) model.saved)
      ]
  }

spec :: Spec Unit
spec = describe "View" do
  it "fills, submits through the form, and queries accessibly" do
    Test.run program do
      expect (Locator.label "Note") (HasDescription "Short and sweet.")
      fill (Locator.label "Note") "milk"
      expect (Locator.label "Note") (HasValue "milk")
      submit (named Button "Save")
      expectModel _.saved [ "milk" ]
      expectPresent (within (role Status) (named Heading "milk"))
      expectCount 1 (role Article)
      expectAbsent (named Heading "eggs")

  it "names the locator it could not find" do
    failsWith "role button named \"Nope\"" $ Test.evaluate program do
      expectPresent (named Button "Nope")

  it "renders escaped HTML with typed roles" do
    renderToString (div [ title_ "\"q\"", role_ Alert ] [ text "<b>" ])
      `shouldEqual` "<div title=\"&quot;q&quot;\" role=\"alert\">&lt;b&gt;</div>"
