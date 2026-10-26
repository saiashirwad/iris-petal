module Test.Command where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as Str
import Foldkit.Command (CommandName(..), Definition, command, define)
import Foldkit.Command as Command
import Foldkit.Core (Update, continue)
import Foldkit.Html (Html, div, text)
import Foldkit.Locator as Locator
import Foldkit.Scene as Scene
import Foldkit.Story as Story
import Foldkit.Test (Test, assertRight, assertTrue, test)

data Message = Sent | Completed String

derive instance eqMessage :: Eq Message

instance showMessage :: Show Message where
  show = case _ of
    Sent -> "Sent"
    Completed value -> "Completed " <> show value

ping :: Definition String String Message
ping = define (CommandName "Ping") (\value -> pure value) Completed

type Model = { sent :: Int }

initialModel :: Model
initialModel = { sent: 0 }

update :: Model -> Message -> Update Model Message Void
update model = case _ of
  Sent ->
    { model: model { sent = model.sent + 1 }
    , commands: [ command ping "a", command ping "b" ]
    , outMessage: Nothing
    }
  Completed _ -> continue model

duplicateUpdate :: Model -> Message -> Update Model Message Void
duplicateUpdate model = case _ of
  Sent -> { model, commands: [ command ping "same", command ping "same" ], outMessage: Nothing }
  Completed _ -> continue model

view :: Model -> Html Message
view model = div [] [ text (show model.sent) ]

tests :: Array Test
tests =
  [ test "exact matchers are order-independent and resolve one instance at a time" do
      assertRight "exact matchers"
        ( Story.runStory update initialModel do
            Story.send Sent
            Story.expectCommands
              [ Command.byInstance (command ping "b"), Command.byInstance (command ping "a") ]
            Story.resolveExact (command ping "a") (Completed "a")
            Story.resolveExact (command ping "b") (Completed "b")
            Story.expectNone
        )
  , test "definition matchers ignore arguments" do
      assertRight "definition matchers"
        ( Story.runStory duplicateUpdate initialModel do
            Story.send Sent
            Story.expectCommands [ Command.byDefinition ping, Command.byDefinition ping ]
            Story.resolveExact (command ping "same") (Completed "same")
            Story.resolveExact (command ping "same") (Completed "same")
            Story.expectNone
        )
  , test "resolving a definition with duplicate matches is ambiguous" do
      case
        Story.runStory duplicateUpdate initialModel do
          Story.send Sent
          Story.resolve ping "same"
      of
        Left errorMessage -> assertTrue "message explains ambiguity"
          (Str.contains (Str.Pattern "multiple pending Commands") errorMessage)
        Right _ -> assertTrue "expected ambiguity to fail" false
  , test "an instance matcher rejects different arguments" do
      case
        Story.runStory update initialModel do
          Story.send Sent
          Story.expectCommands
            [ Command.byInstance (command ping "c"), Command.byInstance (command ping "a") ]
      of
        Left errorMessage -> assertTrue "message shows the exact-command mismatch"
          (Str.contains (Str.Pattern "Expected exactly these Commands") errorMessage)
        Right _ -> assertTrue "expected the exact assertion to fail" false
  , test "expectHasCommands checks a subset" do
      assertRight "subset assertion"
        ( Story.runStory update initialModel do
            Story.send Sent
            Story.expectHasCommands [ Command.byDefinition ping ]
            Story.resolveExact (command ping "a") (Completed "a")
            Story.resolveExact (command ping "b") (Completed "b")
            Story.expectNone
        )
  , test "Scene.send injects an external message and keeps the pending guard" do
      assertRight "scene send"
        ( Scene.runScene update view initialModel do
            Scene.send Sent
            Scene.expectText (Locator.text "1") "1"
            Scene.expectHasCommands [ Command.byDefinition ping ]
            Scene.resolveExact (command ping "a") (Completed "a")
            Scene.resolveExact (command ping "b") (Completed "b")
            Scene.expectNone
        )
  , test "Scene.send is rejected while a Command is pending" do
      case
        Scene.runScene update view initialModel do
          Scene.send Sent
          Scene.send Sent
      of
        Left errorMessage -> assertTrue "message explains the pending command"
          (Str.contains (Str.Pattern "unresolved Commands") errorMessage)
        Right _ -> assertTrue "expected Scene.send to fail" false
  ]
