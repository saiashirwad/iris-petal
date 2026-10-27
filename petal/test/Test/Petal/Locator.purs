module Test.Petal.Locator (spec) where

import Prelude

import Petal (Program)
import Petal.Html
  ( Attribute
  , Html
  , Role(..)
  , a
  , alt_
  , ariaLabel
  , attr
  , button
  , div
  , for_
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , href_
  , id_
  , img
  , input
  , label
  , li
  , nav
  , p
  , placeholder_
  , role_
  , section
  , select
  , span
  , text
  , textarea
  , title_
  , type_
  , ul
  , value_
  )
import Petal.Locator (altText, displayValue, named, nth, placeholder, textContaining, within)
import Petal.Locator as Locator
import Petal.Test (Check(..), evaluate, expect, expectAbsent, expectCount, expectPresent)
import Petal.Test as Test
import Test.Petal.Scenario (failsWith)
import Test.Spec (Spec, describe, it)

page :: Html Unit -> Program Unit Unit
page html = { model: unit, init: pure unit, update: \_ -> pure unit, view: \_ -> html }

labelledBy :: String -> Attribute Unit
labelledBy = attr "aria-labelledby"

spec :: Spec Unit
spec = describe "Locator" do
  describe "builders" do
    it "finds by role, and by role with an accessible name" do
      Test.run (page (div [] [ button [] [ text "Save" ], button [] [ text "Cancel" ] ])) do
        expectCount 2 (Locator.role Button)
        expectCount 1 (named Button "Save")
        expectAbsent (named Button "Delete")

    it "finds an element by its label" do
      Test.run (page (div [] [ label [ for_ "email" ] [ text "Email" ], input [ id_ "email" ] ])) do
        expectPresent (Locator.label "Email")
        expectAbsent (Locator.label "Phone")

    it "finds the innermost element holding exact text, ignoring whitespace" do
      Test.run (page (div [] [ p [] [ text "  Hello \n  world " ], p [] [ text "Hello there" ] ]))
        do
          expectCount 1 (Locator.text "Hello world")
          expect (Locator.text "Hello world") (HasText "Hello world")
          expectAbsent (Locator.text "Hello")

    it "finds elements whose text contains a fragment" do
      Test.run (page (div [] [ p [] [ text "Hello world" ], p [] [ text "Hello there" ] ])) do
        expectCount 2 (textContaining "Hello")
        expectCount 1 (textContaining "world")
        expectAbsent (textContaining "goodbye")

    it "finds an input by placeholder" do
      Test.run
        (page (div [] [ input [ placeholder_ "Search" ], textarea [ placeholder_ "Bio" ] [] ])) do
          expectCount 1 (placeholder "Search")
          expectPresent (placeholder "Bio")
          expectAbsent (placeholder "Nope")

    it "finds an image by alt text" do
      Test.run (page (div [] [ img [ alt_ "Logo" ], img [ alt_ "Banner" ] ])) do
        expectCount 1 (altText "Logo")
        expectAbsent (altText "Avatar")

    it "finds a field by its displayed value, not other elements" do
      Test.run
        ( page
            ( div []
                [ input [ value_ "milk" ]
                , textarea [ value_ "notes" ] []
                , button [ value_ "milk" ] [ text "Go" ]
                ]
            )
        )
        do
          expectCount 1 (displayValue "milk")
          expectPresent (displayValue "notes")
          expectAbsent (displayValue "eggs")

    it "searches within the first match of the parent only" do
      Test.run
        ( page
            ( div []
                [ nav [] [ a [ href_ "/a" ] [ text "A" ] ]
                , nav [] [ a [ href_ "/b" ] [ text "B" ] ]
                , a [ href_ "/c" ] [ text "C" ]
                ]
            )
        )
        do
          expectCount 3 (Locator.role Link)
          expectCount 1 (within (Locator.role Navigation) (Locator.role Link))
          expectPresent (within (Locator.role Navigation) (named Link "A"))
          expectAbsent (within (Locator.role Navigation) (named Link "B"))
          expectAbsent (within (Locator.role Navigation) (named Link "C"))

    it "picks the nth match, counting from zero" do
      Test.run (page (ul [] [ li [] [ text "one" ], li [] [ text "two" ], li [] [ text "three" ] ]))
        do
          expect (nth 0 (Locator.role ListItem)) (HasText "one")
          expect (nth 2 (Locator.role ListItem)) (HasText "three")
          expectAbsent (nth 3 (Locator.role ListItem))

    it "composes nth with within" do
      Test.run
        ( page
            ( div []
                [ ul [] [ li [] [ text "a1" ], li [] [ text "a2" ] ]
                , ul [] [ li [] [ text "b1" ] ]
                ]
            )
        )
        do
          expect (within (Locator.role List) (nth 1 (Locator.role ListItem))) (HasText "a2")

  describe "failure messages" do
    let
      view = page (div [] [ button [] [ text "Save" ] ])
      missing locator fragment = it ("names " <> fragment) do
        failsWith fragment $ evaluate view (expectPresent locator)
    missing (Locator.role Link) "role link"
    missing (named Button "Nope") "role button named \"Nope\""
    missing (Locator.label "Email") "label \"Email\""
    missing (Locator.text "Gone") "text \"Gone\""
    missing (textContaining "Gon") "text containing \"Gon\""
    missing (placeholder "Search") "placeholder \"Search\""
    missing (altText "Logo") "alt text \"Logo\""
    missing (displayValue "milk") "display value \"milk\""
    missing (within (Locator.role Navigation) (Locator.role Link))
      "role link within role navigation"
    missing (nth 4 (Locator.role Button)) "match 4 of role button"

    it "names the locator when an absence or count expectation fails" do
      failsWith "role button" $ evaluate view (expectAbsent (Locator.role Button))
      failsWith "role button" $ evaluate view (expectCount 2 (Locator.role Button))

  describe "accessible names" do
    it "uses every id of aria-labelledby, in order" do
      Test.run
        ( page
            ( div []
                [ span [ id_ "first" ] [ text "First" ]
                , span [ id_ "second" ] [ text "Second" ]
                , button [ labelledBy "first second", ariaLabel "ignored" ] [ text "ignored too" ]
                ]
            )
        )
        do
          expect (Locator.role Button) (HasName "First Second")
          expectPresent (named Button "First Second")

    it "uses aria-label over content" do
      Test.run (page (button [ ariaLabel "Close" ] [ text "x" ])) do
        expectPresent (named Button "Close")
        expectAbsent (named Button "x")

    it "uses a label pointing at the input with for" do
      Test.run
        (page (div [] [ label [ for_ "email" ] [ text "Email address" ], input [ id_ "email" ] ]))
        do
          expect (Locator.role TextBox) (HasName "Email address")

    it "uses a label that wraps its input" do
      Test.run (page (label [] [ text "Full name", input [ id_ "name" ] ])) do
        expect (Locator.role TextBox) (HasName "Full name")

    it "does not name an input from a label that points elsewhere" do
      Test.run
        ( page
            ( div []
                [ label [ for_ "other" ] [ text "Other" ]
                , input [ id_ "email" ]
                , input [ id_ "other" ]
                ]
            )
        )
        do
          expect (nth 0 (Locator.role TextBox)) (HasName "")
          expect (nth 1 (Locator.role TextBox)) (HasName "Other")

    it "names an image from its alt text" do
      Test.run (page (img [ alt_ "Company logo" ])) do
        expect (Locator.role Img) (HasName "Company logo")

    it "names submit, button and reset inputs from their value" do
      Test.run
        ( page
            ( div []
                [ input [ type_ "submit", value_ "Send" ]
                , input [ type_ "button", value_ "Tap" ]
                , input [ type_ "reset", value_ "Clear" ]
                ]
            )
        )
        do
          expectPresent (named Button "Send")
          expectPresent (named Button "Tap")
          expectPresent (named Button "Clear")

    it "names a button, heading and link from their content" do
      Test.run
        ( page
            ( div []
                [ button [] [ text "Save" ]
                , h2 [] [ span [] [ text "Big" ], text " news" ]
                , a [ href_ "/x" ] [ text "Read   more" ]
                ]
            )
        )
        do
          expectPresent (named Button "Save")
          expectPresent (named Heading "Big news")
          expectPresent (named Link "Read more")

    it "does not name a plain container from its content" do
      Test.run (page (div [ role_ Status ] [ text "Saved" ])) do
        expect (Locator.role Status) (HasName "")

    it "falls back to title when nothing else names the element" do
      Test.run
        ( page
            ( div []
                [ button [ title_ "Close" ] []
                , a [ href_ "/x", title_ "Home" ] []
                , button [ title_ "Ignored" ] [ text "Real" ]
                ]
            )
        )
        do
          expectPresent (named Button "Close")
          expectPresent (named Link "Home")
          expectPresent (named Button "Real")
          expectAbsent (named Button "Ignored")

    it "matches a name regardless of whitespace" do
      Test.run (page (button [] [ text "  Save \n now " ])) do
        expectPresent (named Button "Save now")
        expectPresent (named Button "  Save   now")

  describe "label locator" do
    it "finds any element carrying aria-label" do
      Test.run (page (div [ ariaLabel "Toolbar" ] [])) do
        expectPresent (Locator.label "Toolbar")

    it "finds an element named by aria-labelledby" do
      Test.run (page (div [] [ span [ id_ "t" ] [ text "Toolbar" ], div [ labelledBy "t" ] [] ])) do
        expectCount 1 (Locator.label "Toolbar")

    it "does not find unlabelled containers by their content" do
      Test.run (page (div [] [ text "Toolbar" ])) do
        expectAbsent (Locator.label "Toolbar")

  describe "implicit roles" do
    it "gives inputs a role from their type" do
      Test.run
        ( page
            ( div []
                [ input []
                , input [ type_ "text" ]
                , input [ type_ "search" ]
                , input [ type_ "checkbox" ]
                , input [ type_ "radio" ]
                , input [ type_ "number" ]
                , input [ type_ "range" ]
                , input [ type_ "submit" ]
                , input [ type_ "hidden" ]
                ]
            )
        )
        do
          expectCount 2 (Locator.role TextBox)
          expectCount 1 (Locator.role SearchBox)
          expectCount 1 (Locator.role Checkbox)
          expectCount 1 (Locator.role Radio)
          expectCount 1 (Locator.role SpinButton)
          expectCount 1 (Locator.role Slider)
          expectCount 1 (Locator.role Button)

    it "gives select and textarea their roles" do
      Test.run
        (page (div [] [ select [] [], select [ attr "multiple" "true" ] [], textarea [] [] ])) do
          expectCount 1 (Locator.role Combobox)
          expectCount 1 (Locator.role ListBox)
          expectCount 1 (Locator.role TextBox)

    it "gives an image a role only when it has alt text" do
      Test.run (page (div [] [ img [ alt_ "Logo" ], img [] ])) do
        expectCount 1 (Locator.role Img)

    it "gives an anchor a role only when it has an href" do
      Test.run (page (div [] [ a [ href_ "/x" ] [ text "Go" ], a [] [ text "Nowhere" ] ])) do
        expectCount 1 (Locator.role Link)

    it "gives a section a role only when it is labelled" do
      Test.run
        ( page
            ( div []
                [ section [] [ text "Plain" ]
                , section [ ariaLabel "News" ] [ text "Labelled by aria-label" ]
                , span [ id_ "t" ] [ text "Sports" ]
                , section [ labelledBy "t" ] [ text "Labelled by id" ]
                ]
            )
        )
        do
          expectCount 2 (Locator.role Region)
          expectPresent (named Region "News")
          expectPresent (named Region "Sports")

    it "gives every heading level the heading role" do
      Test.run
        ( page
            ( div []
                [ h1 [] [ text "1" ]
                , h2 [] [ text "2" ]
                , h3 [] [ text "3" ]
                , h4 [] [ text "4" ]
                , h5 [] [ text "5" ]
                , h6 [] [ text "6" ]
                ]
            )
        )
        do
          expectCount 6 (Locator.role Heading)
          expectPresent (named Heading "4")

    it "lets an explicit role replace the implicit one" do
      Test.run
        (page (div [] [ div [ role_ Alert ] [ text "Oops" ], button [ role_ Tab ] [ text "One" ] ]))
        do
          expectPresent (named Alert "")
          expectPresent (named Tab "One")
          expectAbsent (Locator.role Button)
