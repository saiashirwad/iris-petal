-- | Scenario tests: drive a `Program` through messages, interactions on its
-- | rendered view, and command results, without a DOM or any effect.
-- |
-- | ```purescript
-- | it "loads a detail" do
-- |   Test.run Pokedex.program do
-- |     resolve Pokedex.fetchPokemonPage (Right { items: summaries, next: Nothing })
-- |     click (named Button "Charmander")
-- |     resolve Pokedex.fetchPokemonDetail (Right charmander)
-- |     expect (role Article) (ContainsText "Fire")
-- | ```
-- |
-- | A scenario starts where the program starts: `program.model` after
-- | `program.init`, with its commands pending. Commands follow one rule: a new
-- | message (sent or dispatched by an interaction) cannot arrive while a
-- | command is pending, and the scenario fails if any command is still
-- | pending at the end. So every command `update` issues is accounted for by a
-- | `resolve`, and there is no separate "expect these commands" step.
module Petal.Test
  ( Scenario
  , Session
  , Observation
  , start
  , step
  , observe
  , finish
  , resolveAt
  , Check(..)
  , run
  , runConcurrent
  , evaluate
  , send
  , resolve
  , resolveFor
  , expectModel
  , click
  , fill
  , submit
  , press
  , toggle
  , expectPresent
  , expectAbsent
  , expectCount
  , expect
  )
  where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (State, gets, modify_, runState)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Petal (Program, Update, runUpdate)
import Petal.Command (Command, Definition)
import Petal.Command.Internal as Command
import Petal.Html (Attribute(..), Element, Html, KeyEvent)
import Petal.Locator (Locator, Match)
import Petal.Locator as Locator

newtype Scenario model message a =
  Scenario (ReaderT (Env model message) (ExceptT String (State (World model message))) a)

derive newtype instance functorScenario :: Functor (Scenario model message)
derive newtype instance applyScenario :: Apply (Scenario model message)
derive newtype instance applicativeScenario :: Applicative (Scenario model message)
derive newtype instance bindScenario :: Bind (Scenario model message)
derive newtype instance monadScenario :: Monad (Scenario model message)

type Env model message =
  { update :: message -> Update model message Unit
  , view :: model -> Html message
  , mode :: Mode
  }

-- | `Serial` rejects a new message while a command is pending; `Overlapping` allows it.
data Mode = Serial | Overlapping

derive instance eqMode :: Eq Mode

type World model message = { model :: model, pending :: Array (Command message) }

-- | A pure, resumable scenario. Unlike `run`, a session permits interactions
-- | while commands are pending, so callers can explore response orderings.
newtype Session model message = Session { env :: Env model message, world :: World model message }

type Observation = { view :: String, pending :: Array String }

start :: forall model message. Program model message -> Session model message
start = begin Overlapping

begin :: forall model message. Mode -> Program model message -> Session model message
begin mode program =
  let
    initial = runUpdate program.init program.model
  in
    Session
      { env: { update: program.update, view: program.view, mode }
      , world: { model: initial.model, pending: initial.commands }
      }

-- | Apply a scenario action atomically. A failed action leaves the input session usable.
step ::
  forall model message a.
  Scenario model message a -> Session model message -> Either String (Session model message)
step (Scenario action) (Session session) =
  case runState (runExceptT (runReaderT action session.env)) session.world of
    Tuple (Left problem) _ -> Left problem
    Tuple (Right _) world -> Right (Session (session { world = world }))

-- | Render the current pure view and list pending commands in resolution order.
observe :: forall model message. Session model message -> Observation
observe (Session session) =
  { view: Locator.snapshot (session.env.view session.world.model)
  , pending: map Command.describe session.world.pending
  }

finish :: forall model message. Session model message -> Either String Unit
finish (Session session) = unresolved session.world.pending

data Check
  = HasText String
  | ContainsText String
  | HasValue String
  | HasAttribute String String
  | HasName String
  | HasDescription String
  | IsDisabled
  | IsEnabled
  | IsChecked

-- | Run a scenario in any `MonadThrow Error` monad (`Aff` in a spec), failing
-- | with the scenario's message.
run ::
  forall m model message.
  MonadThrow Error m => Program model message -> Scenario model message Unit -> m Unit
run program scenario = either (throwError <<< error) pure (evaluate program scenario)

-- | Run a complete scenario allowing messages while commands are pending.
-- | All commands must still be resolved before the scenario ends.
runConcurrent ::
  forall m model message.
  MonadThrow Error m => Program model message -> Scenario model message Unit -> m Unit
runConcurrent program scenario =
  either (throwError <<< error) pure (step scenario (start program) >>= finish)

-- | The pure form of `run`, for asserting on a failure message.
evaluate ::
  forall model message. Program model message -> Scenario model message Unit -> Either String Unit
evaluate program scenario = step scenario (begin Serial program) >>= finish

send :: forall model message. message -> Scenario model message Unit
send = deliver "when you sent a new Message"

-- | Resolve the one pending command from `definition` with its result. Fails
-- | when none is pending, or when several are and `resolveFor` is needed.
resolve ::
  forall model message args result. Definition args result -> result -> Scenario model message Unit
resolve definition =
  resolveOnly (Command.isFrom definition) "resolveFor with the arguments" definition

-- | Resolve the pending command from `definition` issued with `args`.
resolveFor ::
  forall model message args result.
  Eq args => Definition args result -> args -> result -> Scenario model message Unit
resolveFor definition args =
  resolveOnly issuedWith "resolveAt with the pending index" definition
  where
  issuedWith commandValue = Command.argumentsFor definition commandValue == Just args

-- | Resolve a specific pending instance by its zero-based index in `observe`.
-- | Indices refer to the current observation and change when commands settle.
resolveAt ::
  forall model message args result.
  Int -> Definition args result -> result -> Scenario model message Unit
resolveAt index definition result = do
  pending <- Scenario (gets _.pending)
  case Array.index pending index >>= Command.resolveWith definition result of
    Nothing -> failure ("No matching Command at pending index " <> show index <> ".")
    Just message -> do
      Scenario (modify_ _ { pending = fromMaybe pending (Array.deleteAt index pending) })
      apply message

-- | Compare a projection of the model: `expectModel _.search "char"`.
expectModel ::
  forall model message value.
  Eq value => Show value => (model -> value) -> value -> Scenario model message Unit
expectModel project expected = do
  actual <- Scenario (gets (project <<< _.model))
  unless (actual == expected) $ failure
    ( "Expected the model to give\n\n    "
        <> show expected
        <> "\n\nbut it gave\n\n    "
        <> show actual
    )

click :: forall model message. Locator -> Scenario model message Unit
click locator = do
  match <- locate locator
  case bubble clickHandler match <|> submitFromButton match of
    Just message -> deliver "when a click dispatched a new Message" message
    Nothing ->
      failure ("I found " <> Locator.describe locator <> " but nothing handles a click on it.")
  where
  clickHandler = case _ of
    OnClick message -> Just message
    _ -> Nothing
  submitFromButton match =
    if isSubmitButton match.element then bubble submitHandler match else Nothing

fill :: forall model message. Locator -> String -> Scenario model message Unit
fill locator value = do
  match <- locate locator
  case bubble inputHandler match of
    Just handler -> deliver "when typing dispatched a new Message" (handler value)
    Nothing -> failure ("I found " <> Locator.describe locator <> " but it has no input handler.")
  where
  inputHandler = case _ of
    OnInput handler -> Just handler
    _ -> Nothing

submit :: forall model message. Locator -> Scenario model message Unit
submit locator = do
  match <- locate locator
  case bubble submitHandler match of
    Just message -> deliver "when a submit dispatched a new Message" message
    Nothing ->
      failure ("I found " <> Locator.describe locator <> " but no form around it handles submit.")

press :: forall model message. Locator -> String -> Scenario model message Unit
press locator key = do
  match <- locate locator
  case bubble keyHandler match of
    Just handler -> deliver "when a key press dispatched a new Message" (handler (keyEvent key))
    Nothing -> failure ("I found " <> Locator.describe locator <> " but it has no keydown handler.")
  where
  keyHandler = case _ of
    OnKeyDown handler -> Just handler
    _ -> Nothing

-- | Flip a checkbox or switch, as a click on it would.
toggle :: forall model message. Locator -> Scenario model message Unit
toggle locator = do
  match <- locate locator
  case bubble checkHandler match of
    Just handler -> deliver "when a toggle dispatched a new Message"
      (handler (not (Locator.isChecked match.element)))
    Nothing -> failure ("I found " <> Locator.describe locator <> " but it has no change handler.")
  where
  checkHandler = case _ of
    OnCheck handler -> Just handler
    _ -> Nothing

expectPresent :: forall model message. Locator -> Scenario model message Unit
expectPresent = void <<< locate

expectAbsent :: forall model message. Locator -> Scenario model message Unit
expectAbsent locator = do
  found <- findAll locator
  unless (Array.null found) $ failure
    ( "Expected no element matching "
        <> Locator.describe locator
        <> " but found "
        <> show (Array.length found)
        <> "."
    )

expectCount :: forall model message. Int -> Locator -> Scenario model message Unit
expectCount expected locator = do
  actual <- Array.length <$> findAll locator
  unless (actual == expected) $ failure
    ( "Expected "
        <> show expected
        <> " elements matching "
        <> Locator.describe locator
        <> " but found "
        <> show actual
        <> "."
    )

expect :: forall model message. Locator -> Check -> Scenario model message Unit
expect locator check = do
  match <- locate locator
  root <- currentView
  let
    element = match.element
    compare what wanted actual =
      if actual == wanted
      then Nothing
      else Just (what <> " \"" <> wanted <> "\" but it is \"" <> actual <> "\"")
    attributeIs name wanted = case Locator.attribute name element of
      Nothing -> Just ("attribute " <> name <> "=\"" <> wanted <> "\" but it has none")
      Just actual -> compare ("attribute " <> name <> " to be") wanted actual
    problem = case check of
      HasText wanted -> compare "text" wanted (Locator.textOf element)
      ContainsText wanted ->
        if Str.contains (Str.Pattern wanted) (Locator.textOf element)
        then Nothing
        else Just
          ( "text containing \""
              <> wanted
              <> "\" but its text is \""
              <> Locator.textOf element
              <> "\""
          )
      HasValue wanted -> attributeIs "value" wanted
      HasAttribute name wanted -> attributeIs name wanted
      HasName wanted -> compare "accessible name" wanted (Locator.accessibleName root element)
      HasDescription wanted ->
        compare "accessible description" wanted (Locator.accessibleDescription root element)
      IsDisabled ->
        if Locator.isDisabled element then Nothing else Just "to be disabled but it is enabled"
      IsEnabled ->
        if Locator.isDisabled element then Just "to be enabled but it is disabled" else Nothing
      IsChecked -> if Locator.isChecked element then Nothing else Just "to be checked but it is not"
  case problem of
    Nothing -> pure unit
    Just detail -> failure ("Expected " <> Locator.describe locator <> " to have " <> detail <> ".")

deliver :: forall model message. String -> message -> Scenario model message Unit
deliver context message = do
  pending <- Scenario (gets _.pending)
  mode <- Scenario (asks _.mode)
  if mode == Serial && not (Array.null pending) then failure
    ( "I found unresolved Commands "
        <> context
        <> ":\n\n"
        <> listCommands pending
        <> "\n\nResolve every pending Command before the next Message."
    )
  else apply message

apply :: forall model message. message -> Scenario model message Unit
apply message = do
  update <- Scenario (asks _.update)
  Scenario $ modify_ \world ->
    let
      next = runUpdate (update message) world.model
    in
      { model: next.model, pending: world.pending <> next.commands }

-- | Resolve the one pending command `chosen` selects, naming `alternative` when several match.
resolveOnly ::
  forall model message args result.
  (Command message -> Boolean) ->
  String ->
  Definition args result ->
  result ->
  Scenario model message Unit
resolveOnly chosen alternative definition result = do
  pending <- Scenario (gets _.pending)
  case Array.filter chosen pending, Array.findIndex chosen pending of
    [ _ ], Just index -> resolveAt index definition result
    [], _ -> failure (notPending definition pending)
    many, _ -> failure
      ( "I tried to resolve "
          <> Command.definitionName definition
          <> " but several are pending:\n\n"
          <> listCommands many
          <> "\n\nUse "
          <> alternative
          <> " of the one you mean."
      )

notPending ::
  forall message args result. Definition args result -> Array (Command message) -> String
notPending definition pending = "I tried to resolve "
  <> name
  <> " but none is pending.\n\nPending Commands:\n\n"
  <> listCommands pending
  <> hint
  where
  name = Command.definitionName definition
  hint =
    if Array.any (\commandValue -> Command.describe commandValue `startsWith` (name <> " ")) pending
    then
      "\n\nA pending Command has that name but comes from a different Definition value. Define each Command at the top level of a module and resolve it with that value."
    else ""
  startsWith value prefix = Str.take (Str.length prefix) value == prefix

unresolved :: forall message. Array (Command message) -> Either String Unit
unresolved pending =
  if Array.null pending
  then Right unit
  else Left ("The scenario ended with unresolved Commands:\n\n" <> listCommands pending)

listCommands :: forall message. Array (Command message) -> String
listCommands = case _ of
  [] -> "    (none)"
  commands ->
    intercalate "\n" (map (\commandValue -> "    " <> Command.describe commandValue) commands)

locate :: forall model message. Locator -> Scenario model message (Match message)
locate locator = do
  found <- findAll locator
  case found of
    [ match ] -> pure match
    [] -> failure
      ( "I could not find an element matching "
          <> Locator.describe locator
          <> ".\n\nCheck that the view renders it in the current state."
      )
    _ -> failure
      ( "Found "
          <> show (Array.length found)
          <> " elements matching "
          <> Locator.describe locator
          <> ". Use within or nth to choose one."
      )

findAll :: forall model message. Locator -> Scenario model message (Array (Match message))
findAll locator = Locator.findAll locator <$> currentView

currentView :: forall model message. Scenario model message (Html message)
currentView = do
  view <- Scenario (asks _.view)
  model <- Scenario (gets _.model)
  pure (view model)

failure :: forall model message a. String -> Scenario model message a
failure = Scenario <<< throwError

bubble :: forall message value. (Attribute message -> Maybe value) -> Match message -> Maybe value
bubble handler match = Array.findMap (\element -> Array.findMap handler element.attributes)
  (Array.cons match.element (Array.reverse match.ancestors))

submitHandler :: forall message. Attribute message -> Maybe message
submitHandler = case _ of
  OnSubmit message -> Just message
  _ -> Nothing

isSubmitButton :: forall message. Element message -> Boolean
isSubmitButton element = case element.tag of
  "button" -> Locator.attribute "type" element `Array.elem` [ Nothing, Just "submit" ]
  "input" -> Locator.attribute "type" element == Just "submit"
  _ -> false

keyEvent :: String -> KeyEvent
keyEvent key = { key, shiftKey: false, ctrlKey: false, altKey: false, metaKey: false }
