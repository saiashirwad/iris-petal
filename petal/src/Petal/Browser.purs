-- | The browser runtime: mount a `Program` into a container, patch the DOM
-- | after every update, and run commands.
-- |
-- | The diff compares elements and text only (`Html` has no fragments).
-- | Children are matched by `key` when any sibling has one, by position
-- | otherwise. Every attribute, including `onVisible`, is patched in place, so
-- | a reused node keeps its focus, selection, and observers.
module Petal.Browser (mount) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Petal (Program, runUpdate)
import Petal.Command (Command)
import Petal.Command.Internal as Command
import Petal.Html (Attribute(..), Element, Html(..), KeyEvent, roleName)

foreign import data Node :: Type
foreign import data DomEvent :: Type

type FocusState = { id :: String, start :: Int, end :: Int }

foreign import captureFocus :: Effect FocusState
foreign import restoreFocus :: FocusState -> Effect Unit
foreign import requireElementById :: String -> Effect Node
foreign import createElement :: String -> Effect Node
foreign import createTextNode :: String -> Effect Node
foreign import childNodes :: Node -> Effect (Array Node)
foreign import appendChild :: Node -> Node -> Effect Unit
foreign import insertAt :: Node -> Node -> Int -> Effect Unit
foreign import removeChild :: Node -> Node -> Effect Unit
foreign import replaceChild :: Node -> Node -> Node -> Effect Unit
foreign import clearChildren :: Node -> Effect Unit
foreign import setAttribute :: Node -> String -> String -> Effect Unit
foreign import removeAttribute :: Node -> String -> Effect Unit
foreign import setValue :: Node -> String -> Effect Unit
foreign import setNodeValue :: Node -> String -> Effect Unit
foreign import setEventListener :: Node -> String -> (DomEvent -> Effect Unit) -> Effect Unit
foreign import removeEventListener :: Node -> String -> Effect Unit
foreign import setVisibleHandler :: Node -> Effect Unit -> Effect Unit
foreign import removeVisibleHandler :: Node -> Effect Unit
foreign import eventValue :: DomEvent -> String
foreign import eventChecked :: DomEvent -> Boolean
foreign import eventKey :: DomEvent -> KeyEvent

type Runtime model message =
  { program :: Program model message
  , container :: Node
  , model :: Ref model
  , rendered :: Ref (Array (Html message))
  }

mount :: forall model message. String -> Program model message -> Effect Unit
mount containerId program = do
  container <- requireElementById containerId
  clearChildren container
  let
    start = runUpdate program.init program.model
  model <- Ref.new start.model
  rendered <- Ref.new []
  let
    runtime = { program, container, model, rendered }
  render runtime
  runCommands runtime start.commands

dispatch :: forall model message. Runtime model message -> message -> Effect Unit
dispatch runtime message = do
  model <- Ref.read runtime.model
  let
    step = runUpdate (runtime.program.update message) model
  Ref.write step.model runtime.model
  render runtime
  runCommands runtime step.commands

render :: forall model message. Runtime model message -> Effect Unit
render runtime = do
  model <- Ref.read runtime.model
  previous <- Ref.read runtime.rendered
  let
    next = [ runtime.program.view model ]
  focus <- captureFocus
  patchChildren (dispatch runtime) runtime.container previous next
  Ref.write next runtime.rendered
  restoreFocus focus

runCommands :: forall model message. Runtime model message -> Array (Command message) -> Effect Unit
runCommands runtime = traverse_ \commandValue -> launchAff_ do
  message <- Command.run commandValue
  liftEffect (dispatch runtime message)

type Dispatch message = message -> Effect Unit

patchChildren ::
  forall message.
  Dispatch message -> Node -> Array (Html message) -> Array (Html message) -> Effect Unit
patchChildren send parent old new =
  if Array.any hasKey old || Array.any hasKey new
  then patchKeyed send parent old new
  else patchPositional send parent old new

patchPositional ::
  forall message.
  Dispatch message -> Node -> Array (Html message) -> Array (Html message) -> Effect Unit
patchPositional send parent old new = do
  nodes <- childNodes parent
  for_ (Array.zip nodes (Array.zip old new)) \(Tuple node (Tuple before after)) ->
    patchNode send parent node before after
  for_ (Array.drop (Array.length old) new) \child -> createNode send child >>= appendChild parent
  for_ (Array.drop (Array.length new) nodes) (removeChild parent)

patchKeyed ::
  forall message.
  Dispatch message -> Node -> Array (Html message) -> Array (Html message) -> Effect Unit
patchKeyed send parent old new = do
  nodes <- childNodes parent
  let
    entries = Array.zip nodes old
    byKey = Map.fromFoldable
      ( Array.mapMaybe (\(Tuple node html) -> (\key -> Tuple key { node, html }) <$> keyOf html)
          entries
      )
  reused <- Ref.new Set.empty
  forWithIndex_ new \index child -> do
    used <- Ref.read reused
    node <- case keyOf child >>= \key -> map (Tuple key) (Map.lookup key byKey) of
      Just (Tuple key previous)
        | not (Set.member key used),
          Element before <- previous.html,
          Element after <- child,
          before.tag == after.tag -> do
          Ref.modify_ (Set.insert key) reused
          patchElement send previous.node before after
          pure previous.node
      _ -> createNode send child
    insertAt parent node index
  used <- Ref.read reused
  for_ entries \(Tuple node html) -> case keyOf html of
    Just key
      | Set.member key used -> pure unit
    _ -> removeChild parent node

patchNode ::
  forall message. Dispatch message -> Node -> Node -> Html message -> Html message -> Effect Unit
patchNode send parent node old new = case old, new of
  Text before, Text after -> when (before /= after) (setNodeValue node after)
  Element before, Element after
    | before.tag == after.tag && keyOf old == keyOf new -> patchElement send node before after
  _, _ -> do
    replacement <- createNode send new
    replaceChild parent replacement node

patchElement ::
  forall message. Dispatch message -> Node -> Element message -> Element message -> Effect Unit
patchElement send node old new = do
  patchAttributes send node old.attributes new.attributes
  patchChildren send node old.children new.children

createNode :: forall message. Dispatch message -> Html message -> Effect Node
createNode send = case _ of
  Text value -> createTextNode value
  Element element -> do
    node <- createElement element.tag
    for_ element.attributes (setAttributeOn send node)
    for_ element.children \child -> createNode send child >>= appendChild node
    pure node

patchAttributes ::
  forall message.
  Dispatch message -> Node -> Array (Attribute message) -> Array (Attribute message) -> Effect Unit
patchAttributes send node old new = do
  for_ (Array.difference (Array.mapMaybe attributeName old) (Array.mapMaybe attributeName new))
    (removeAttribute node)
  for_ (Array.difference (Array.mapMaybe eventName old) (Array.mapMaybe eventName new))
    (removeEventListener node)
  when (Array.any isVisible old && not (Array.any isVisible new)) (removeVisibleHandler node)
  for_ new (setAttributeOn send node)

setAttributeOn :: forall message. Dispatch message -> Node -> Attribute message -> Effect Unit
setAttributeOn send node = case _ of
  Property "value" value -> setAttribute node "value" value *> setValue node value
  Property name value -> setAttribute node name value
  Flag name true -> setAttribute node name ""
  Flag name false -> removeAttribute node name
  AriaRole role -> setAttribute node "role" (roleName role)
  Key _ -> pure unit
  OnClick message -> setEventListener node "click" \_ -> send message
  OnInput handler -> setEventListener node "input" \event -> send (handler (eventValue event))
  OnChange handler -> setEventListener node "change" \event -> send (handler (eventValue event))
  OnCheck handler -> setEventListener node "change" \event -> send (handler (eventChecked event))
  OnSubmit message -> setEventListener node "submit" \_ -> send message
  OnKeyDown handler -> setEventListener node "keydown" \event -> send (handler (eventKey event))
  OnFocus message -> setEventListener node "focus" \_ -> send message
  OnBlur message -> setEventListener node "blur" \_ -> send message
  OnVisible message -> setVisibleHandler node (send message)

keyOf :: forall message. Html message -> Maybe String
keyOf = case _ of
  Element element -> Array.findMap
    ( case _ of
        Key key -> Just key
        _ -> Nothing
    )
    element.attributes
  Text _ -> Nothing

hasKey :: forall message. Html message -> Boolean
hasKey html = keyOf html /= Nothing

isVisible :: forall message. Attribute message -> Boolean
isVisible = case _ of
  OnVisible _ -> true
  _ -> false

attributeName :: forall message. Attribute message -> Maybe String
attributeName = case _ of
  Property name _ -> Just name
  Flag name _ -> Just name
  AriaRole _ -> Just "role"
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
