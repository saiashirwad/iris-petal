module Foldkit.Runtime (Application, runApplication, isBrowser) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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
foreign import childNodes :: Element -> Effect (Array Element)
foreign import firstChild :: Element -> Effect Element
foreign import appendChild :: Element -> Element -> Effect Unit
foreign import removeChild :: Element -> Element -> Effect Unit
foreign import replaceChild :: Element -> Element -> Element -> Effect Unit
foreign import clearChildren :: Element -> Effect Unit
foreign import setAttribute :: Element -> String -> String -> Effect Unit
foreign import removeAttribute :: Element -> String -> Effect Unit
foreign import setProperty :: Element -> String -> String -> Effect Unit
foreign import setNodeValue :: Element -> String -> Effect Unit
foreign import setEventListener :: Element -> String -> (DomEvent -> Effect Unit) -> Effect Unit
foreign import removeEventListener :: Element -> String -> Effect Unit
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
  previousRef <- Ref.new Nothing
  render modelRef previousRef app container (app.program.view initial.model)
  runCommands modelRef previousRef app container initial.commands

render ::
  forall model message input.
  Ref.Ref model ->
  Ref.Ref (Maybe (Html message)) ->
  Application model message input ->
  Element ->
  Html message ->
  Effect Unit
render modelRef previousRef app container html = do
  focus <- captureFocus
  previous <- Ref.read previousRef
  let
    dispatchMessage = dispatch modelRef previousRef app container
  case previous of
    Nothing -> do
      clearChildren container
      renderInto dispatchMessage container html
    Just old -> patchRoot dispatchMessage container old html
  Ref.write (Just html) previousRef
  restoreFocus focus

dispatch ::
  forall model message input.
  Ref.Ref model ->
  Ref.Ref (Maybe (Html message)) ->
  Application model message input ->
  Element ->
  message ->
  Effect Unit
dispatch modelRef previousRef app container message = do
  model <- Ref.read modelRef
  let
    result = app.program.update model message
  Ref.write result.model modelRef
  render modelRef previousRef app container (app.program.view result.model)
  runCommands modelRef previousRef app container result.commands

runCommands ::
  forall model message input.
  Ref.Ref model ->
  Ref.Ref (Maybe (Html message)) ->
  Application model message input ->
  Element ->
  Array (Command message) ->
  Effect Unit
runCommands modelRef previousRef app container commands = for_ commands \commandValue ->
  launchAff_ do
    resultMessage <- Command.execute commandValue
    liftEffect (dispatch modelRef previousRef app container resultMessage)

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

patchRoot ::
  forall message. (message -> Effect Unit) -> Element -> Html message -> Html message -> Effect Unit
patchRoot dispatchMessage parent old new = case old, new of
  Element oldElement, Element newElement
    | oldElement.tag == newElement.tag && reusable oldElement.attributes newElement.attributes -> do
      node <- firstChild parent
      patchElement dispatchMessage node oldElement newElement
  _, _ -> do
    clearChildren parent
    renderInto dispatchMessage parent new

patchElement ::
  forall message.
  (message -> Effect Unit) ->
  Element ->
  { tag :: String, attributes :: Array (Attribute message), children :: Array (Html message) } ->
  { tag :: String, attributes :: Array (Attribute message), children :: Array (Html message) } ->
  Effect Unit
patchElement dispatchMessage node old new = do
  patchAttributes dispatchMessage node old.attributes new.attributes
  patchChildren dispatchMessage node old.children new.children

patchChildren ::
  forall message.
  (message -> Effect Unit) -> Element -> Array (Html message) -> Array (Html message) -> Effect Unit
patchChildren dispatchMessage parent oldChildren newChildren =
  if Array.any isComplex oldChildren || Array.any isComplex newChildren
  then do
    clearChildren parent
    for_ newChildren (renderInto dispatchMessage parent)
  else do
    nodes <- childNodes parent
    for_ (Array.zip nodes (Array.zip oldChildren newChildren))
      \(Tuple node (Tuple oldChild newChild)) ->
        patchNode dispatchMessage parent node oldChild newChild
    let
      oldLength = Array.length oldChildren
      newLength = Array.length newChildren
    when (newLength > oldLength) do
      for_ (Array.drop oldLength newChildren) (renderInto dispatchMessage parent)
    when (oldLength > newLength) do
      for_ (Array.drop newLength nodes) (removeChild parent)

patchNode ::
  forall message.
  (message -> Effect Unit) -> Element -> Element -> Html message -> Html message -> Effect Unit
patchNode dispatchMessage parent node old new = case old, new of
  Text oldText, Text newText -> when (oldText /= newText) (setNodeValue node newText)
  Element oldElement, Element newElement
    | oldElement.tag == newElement.tag && reusable oldElement.attributes newElement.attributes ->
      patchElement dispatchMessage node oldElement newElement
  _, _ -> do
    newNode <- createNode dispatchMessage new
    replaceChild parent newNode node

createNode :: forall message. (message -> Effect Unit) -> Html message -> Effect Element
createNode dispatchMessage = case _ of
  Text value -> createTextNode value
  Element element -> do
    node <- createElement element.tag
    for_ element.attributes (renderAttribute dispatchMessage node)
    for_ element.children (renderInto dispatchMessage node)
    pure node
  Empty -> createTextNode ""
  Fragment _ -> createTextNode ""

patchAttributes ::
  forall message.
  (message -> Effect Unit) ->
  Element ->
  Array (Attribute message) ->
  Array (Attribute message) ->
  Effect Unit
patchAttributes dispatchMessage node old new = do
  let
    oldEvents = Array.mapMaybe eventName old
    newEvents = Array.mapMaybe eventName new
    oldNames = Array.mapMaybe attributeName old
    newNames = Array.mapMaybe attributeName new
  for_ oldEvents \name -> when (not (Array.elem name newEvents)) (removeEventListener node name)
  for_ oldNames \name -> when (not (Array.elem name newNames)) (removeAttribute node name)
  for_ new (renderAttribute dispatchMessage node)

reusable :: forall message. Array (Attribute message) -> Array (Attribute message) -> Boolean
reusable old new = not (Array.any isVisibleAttribute old) && not (Array.any isVisibleAttribute new)

isVisibleAttribute :: forall message. Attribute message -> Boolean
isVisibleAttribute = case _ of
  OnVisible _ -> true
  _ -> false

isComplex :: forall message. Html message -> Boolean
isComplex = case _ of
  Fragment _ -> true
  Empty -> true
  _ -> false

attributeName :: forall message. Attribute message -> Maybe String
attributeName = case _ of
  Attribute name _ -> Just name
  BooleanAttribute name _ -> Just name
  _ -> Nothing

eventName :: forall message. Attribute message -> Maybe String
eventName = case _ of
  OnClick _ -> Just "click"
  OnInput _ -> Just "input"
  OnChange _ -> Just "change"
  OnCheck _ -> Just "change"
  OnSubmit _ -> Just "submit"
  OnKeyDown _ -> Just "keydown"
  OnFocus _ -> Just "focus"
  OnBlur _ -> Just "blur"
  _ -> Nothing

renderAttribute ::
  forall message. (message -> Effect Unit) -> Element -> Attribute message -> Effect Unit
renderAttribute dispatchMessage node = case _ of
  Attribute name value -> do
    setAttribute node name value
    when (name == "value") (setProperty node name value)
  BooleanAttribute name true -> setAttribute node name ""
  BooleanAttribute name false -> removeAttribute node name
  OnClick message -> setEventListener node "click" \_ -> dispatchMessage message
  OnInput handler -> setEventListener node "input" \event ->
    dispatchMessage (handler (eventValue event))
  OnChange handler -> setEventListener node "change" \event ->
    dispatchMessage (handler (eventValue event))
  OnCheck handler -> setEventListener node "change" \event ->
    dispatchMessage (handler (eventChecked event))
  OnSubmit message -> setEventListener node "submit" \_ -> dispatchMessage message
  OnKeyDown handler -> setEventListener node "keydown" \event ->
    dispatchMessage (handler (keyEvent event))
  OnFocus message -> setEventListener node "focus" \_ -> dispatchMessage message
  OnBlur message -> setEventListener node "blur" \_ -> dispatchMessage message
  OnVisible message -> observeVisible node (dispatchMessage message)

keyEvent :: DomEvent -> KeyEvent
keyEvent event =
  { key: eventKey event
  , shiftKey: eventShiftKey event
  , ctrlKey: eventCtrlKey event
  , altKey: eventAltKey event
  , metaKey: eventMetaKey event
  }
