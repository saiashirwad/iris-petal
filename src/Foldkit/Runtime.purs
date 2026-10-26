module Foldkit.Runtime (Application, runApplication, isBrowser) where

import Prelude

import Data.Foldable (for_)
import Data.Void (Void)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foldkit.Command (Command)
import Foldkit.Command as Command
import Foldkit.Core (Program, Update)
import Foldkit.Html (Attribute(..), Html(..), KeyEvent)

type Application model message input =
  { program :: Program model message input Void, input :: input, containerId :: String }

foreign import data Element :: Type
foreign import data DomEvent :: Type

foreign import isBrowser :: Effect Boolean
foreign import captureFocus :: Effect FocusState
foreign import restoreFocus :: FocusState -> Effect Unit
foreign import requireElementById :: String -> Effect Element
foreign import createElement :: String -> Effect Element
foreign import createTextNode :: String -> Effect Element
foreign import appendChild :: Element -> Element -> Effect Unit
foreign import clearChildren :: Element -> Effect Unit
foreign import setAttribute :: Element -> String -> String -> Effect Unit
foreign import removeAttribute :: Element -> String -> Effect Unit
foreign import setProperty :: Element -> String -> String -> Effect Unit
foreign import addEventListener :: Element -> String -> (DomEvent -> Effect Unit) -> Effect Unit
foreign import observeVisible :: Element -> Effect Unit -> Effect Unit
foreign import eventValue :: DomEvent -> String
foreign import eventChecked :: DomEvent -> Boolean
foreign import eventKey :: DomEvent -> String
foreign import eventShiftKey :: DomEvent -> Boolean
foreign import eventCtrlKey :: DomEvent -> Boolean
foreign import eventAltKey :: DomEvent -> Boolean
foreign import eventMetaKey :: DomEvent -> Boolean

type FocusState = { id :: String, start :: Int, end :: Int }

runApplication :: forall model message input. Application model message input -> Effect Unit
runApplication app = do
  container <- requireElementById app.containerId
  let
    initial = app.program.start app.input
  modelRef <- Ref.new initial.model
  render modelRef app container (app.program.view initial.model)
  runCommands modelRef app container initial.commands

render ::
  forall model message input.
  Ref.Ref model -> Application model message input -> Element -> Html message -> Effect Unit
render modelRef app container html = do
  focus <- captureFocus
  clearChildren container
  renderInto (dispatch modelRef app container) container html
  restoreFocus focus

dispatch ::
  forall model message input.
  Ref.Ref model -> Application model message input -> Element -> message -> Effect Unit
dispatch modelRef app container message = do
  model <- Ref.read modelRef
  let
    result = app.program.update model message
  Ref.write result.model modelRef
  render modelRef app container (app.program.view result.model)
  runCommands modelRef app container result.commands

runCommands ::
  forall model message input.
  Ref.Ref model ->
  Application model message input ->
  Element ->
  Array (Command message) ->
  Effect Unit
runCommands modelRef app container commands = for_ commands \commandValue -> launchAff_ do
  resultMessage <- Command.execute commandValue
  liftEffect (dispatch modelRef app container resultMessage)

renderInto :: forall message. (message -> Effect Unit) -> Element -> Html message -> Effect Unit
renderInto dispatchMessage parent = case _ of
  Text value -> do
    node <- createTextNode value
    appendChild parent node
  Empty -> pure unit
  Fragment children -> for_ children (renderInto dispatchMessage parent)
  Element element -> do
    node <- createElement element.tag
    for_ element.attributes (renderAttribute dispatchMessage node)
    for_ element.children (renderInto dispatchMessage node)
    appendChild parent node

renderAttribute ::
  forall message. (message -> Effect Unit) -> Element -> Attribute message -> Effect Unit
renderAttribute dispatchMessage node = case _ of
  Attribute name value -> do
    setAttribute node name value
    when (name == "value") (setProperty node name value)
  BooleanAttribute name true -> setAttribute node name ""
  BooleanAttribute name false -> removeAttribute node name
  OnClick message -> addEventListener node "click" \_ -> dispatchMessage message
  OnInput handler -> addEventListener node "input" \event ->
    dispatchMessage (handler (eventValue event))
  OnChange handler -> addEventListener node "change" \event ->
    dispatchMessage (handler (eventValue event))
  OnCheck handler -> addEventListener node "change" \event ->
    dispatchMessage (handler (eventChecked event))
  OnSubmit message -> addEventListener node "submit" \_ -> dispatchMessage message
  OnKeyDown handler -> addEventListener node "keydown" \event ->
    dispatchMessage (handler (keyEvent event))
  OnFocus message -> addEventListener node "focus" \_ -> dispatchMessage message
  OnBlur message -> addEventListener node "blur" \_ -> dispatchMessage message
  OnVisible message -> observeVisible node (dispatchMessage message)

keyEvent :: DomEvent -> KeyEvent
keyEvent event =
  { key: eventKey event
  , shiftKey: eventShiftKey event
  , ctrlKey: eventCtrlKey event
  , altKey: eventAltKey event
  , metaKey: eventMetaKey event
  }
