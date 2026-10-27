-- | Shared JSON-lines driver for the example applications.
module Example.Explore.Session (run) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Node.EventEmitter (on_)
import Node.Process (stdin)
import Node.ReadLine as ReadLine
import Petal (Program)
import Petal.Html (Role(..))
import Petal.Locator as Locator
import Petal.Test (Scenario)
import Petal.Test as Test

data Action
  = Observe
  | Trace
  | Finish
  | Reset
  | Click String
  | Fill String
  | Resolve Int String

emit :: forall a. EncodeJson a => a -> Effect Unit
emit value = log (stringify (encodeJson value))

parse :: Json -> Either String Action
parse json = do
  object <- decode (decodeJson json)
  op <- decode (object .: "op")
  case op of
    "observe" -> pure Observe
    "trace" -> pure Trace
    "finish" -> pure Finish
    "reset" -> pure Reset
    "click" -> Click <$> decode (object .: "name")
    "fill" -> Fill <$> decode (object .: "value")
    "resolve" -> Resolve <$> decode (object .: "index") <*> decode (object .: "fixture")
    _ -> Left ("Unknown operation: " <> op)
  where
  decode :: forall a. Either JsonDecodeError a -> Either String a
  decode = lmap printJsonDecodeError

run ::
  forall model message.
  Program model message ->
  Maybe String ->
  (Int -> String -> Either String (Scenario model message Unit)) ->
  Effect Unit
run program inputLabel resolveFixture = do
  let
    fresh = { session: Test.start program, trace: [] :: Array Json }
  state <- Ref.new fresh
  let
    reject error = emit { ok: false, error }
    observe = do
      { session } <- Ref.read state
      emit { ok: true, observation: Test.observe session }
    perform json action = do
      { session, trace } <- Ref.read state
      case Test.step action session of
        Left error -> emit { ok: false, error, observation: Test.observe session }
        Right next -> do
          Ref.write { session: next, trace: Array.snoc trace json } state
          emit { ok: true, observation: Test.observe next }
    dispatch json = case _ of
      Observe -> observe
      Trace -> do
        { trace } <- Ref.read state
        emit { ok: true, trace }
      Finish -> do
        { session } <- Ref.read state
        either reject (\_ -> emit { ok: true }) (Test.finish session)
      Reset -> Ref.write fresh state *> observe
      Click name -> perform json (Test.click (Locator.named Button name))
      Fill value -> case inputLabel of
        Nothing -> reject "This example has no configured input."
        Just label -> perform json (Test.fill (Locator.label label) value)
      Resolve index fixture -> either reject (perform json) (resolveFixture index fixture)
    handle line = either reject identity do
      json <- jsonParser line
      dispatch json <$> parse json
  observe
  input <- ReadLine.createInterface stdin mempty
  on_ ReadLine.lineH handle input
