module Test.Foldkit.Scenario (spec, failsWith) where

import Prelude

import Control.Monad.State.Class (modify_)
import Data.Either (Either(..))
import Data.String as Str
import Effect.Aff (Aff)
import Foldkit (Program, perform)
import Foldkit.Command (Definition, define)
import Foldkit.Html (Role(..), button, div, onClick, p, text)
import Foldkit.Locator (named, role)
import Foldkit.Test (Check(..), click, evaluate, expect, expectModel, resolve, resolveFor, send)
import Foldkit.Test as Test
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

data Message = Pinged | Ponged String

ping :: Definition String String
ping = define "Ping" pure

pingAgain :: Definition String String
pingAgain = define "Ping" pure

program :: Program { pongs :: Array String } Message
program =
  { model: { pongs: [] }
  , init: pure unit
  , update: case _ of
      Pinged -> do
        perform ping "a" Ponged
        perform ping "b" Ponged
      Ponged value -> modify_ \model -> model { pongs = model.pongs <> [ value ] }
  , view: \model -> div []
      [ p [] [ text (Str.joinWith "," model.pongs) ], button [ onClick Pinged ] [ text "Ping" ] ]
  }

failsWith :: String -> Either String Unit -> Aff Unit
failsWith fragment = case _ of
  Left message
    | Str.contains (Str.Pattern fragment) message -> pure unit
    | otherwise ->
      fail ("Expected a failure containing \"" <> fragment <> "\" but got:\n" <> message)
  Right _ -> fail ("Expected a failure containing \"" <> fragment <> "\" but the scenario passed")

spec :: Spec Unit
spec = describe "Scenario" do
  it "resolves each command by its arguments" do
    Test.run program do
      click (named Button "Ping")
      resolveFor ping "b" "B"
      resolveFor ping "a" "A"
      expectModel _.pongs [ "B", "A" ]
      expect (role Paragraph) (HasText "B,A")

  it "rejects an ambiguous resolve" do
    failsWith "several are pending" $ evaluate program do
      send Pinged
      resolve ping "x"

  it "rejects a new message while a command is pending" do
    failsWith "unresolved Commands when you sent" $ evaluate program do
      send Pinged
      send Pinged

  it "fails when a command is left unresolved" do
    failsWith "ended with unresolved Commands" $ evaluate program do
      send Pinged
      resolveFor ping "a" "A"

  it "explains a definition that shares a name but not an identity" do
    failsWith "different Definition value" $ evaluate program do
      send Pinged
      resolveFor pingAgain "a" "A"
