module Test.Render where

import Prelude

import Data.String as Str
import Effect.Aff (Aff)
import Example.Counter as Counter
import Foldkit.Html as Html
import Foldkit.Render (renderToString)
import Foldkit.Test (Test, assertTrue, test)

renderTests :: Array Test
renderTests =
  [ test "renders the counter view to escaped HTML" do
      let
        html = renderToString (Counter.view Counter.initialModel)
      assertTrue
        "contains the counter output"
        ( Str.contains (Str.Pattern "<h1>Counter</h1>") html
            && Str.contains (Str.Pattern "<p>0</p>") html
            && Str.contains (Str.Pattern "<button>+</button>") html
        )
  , test "escapes text and attribute values" do
      let
        html =
          renderToString
            ( Html.div
                [ Html.title_ "\"quoted\"" ]
                [ Html.text "<script>" ]
            )
      assertTrue "escapes text" (Str.contains (Str.Pattern "&lt;script&gt;") html)
      assertTrue "escapes attributes" (Str.contains (Str.Pattern "&quot;quoted&quot;") html)
    ]
