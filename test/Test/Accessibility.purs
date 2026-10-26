module Test.Accessibility where

import Prelude

import Data.Void (Void)
import Foldkit.Core (Update, continue)
import Foldkit.Html (Html, ariaDescribedBy, div, id_, input, p, text, type_)
import Foldkit.Query as Query
import Foldkit.Scene as Scene
import Foldkit.Test (Test, assertRight, test)

data Message = Noop

type Model = Unit

update :: Model -> Message -> Update Model Message Void
update model _ = continue model

view :: Model -> Html Message
view _ = div []
  [ input [ id_ "email", type_ "text", ariaDescribedBy "email-help" ]
  , p [ id_ "email-help" ] [ text "We never share it." ]
  ]

tests :: Array Test
tests =
  [ test "accessible description resolves aria-describedby" do
      assertRight "accessible description"
        ( Scene.runScene update view unit do
            Scene.expectAccessibleDescription (Query.role "textbox") "We never share it."
        )
  ]
