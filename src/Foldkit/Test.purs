module Foldkit.Test (Test, test, suite, runTests, assertEqual, assertTrue, assertRight) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), isLeft)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception as Exception
import Effect.Exception (throw)

foreign import setExitCode :: Int -> Effect Unit

type Test = { name :: String, run :: Aff Unit }

test :: String -> Aff Unit -> Test
test name run = { name, run }

suite :: String -> Array Test -> Array Test
suite prefix = map \value -> value { name = prefix <> " › " <> value.name }

runTests :: Array Test -> Aff Unit
runTests tests = do
  results <- for (Array.mapWithIndex Tuple tests) \(Tuple index value) -> do
    result <- try value.run
    liftEffect case result of
      Left error -> log
        ("FAIL " <> show (index + 1) <> ". " <> value.name <> "\n     " <> Exception.message error)
      Right _ -> log ("PASS " <> show (index + 1) <> ". " <> value.name)
    pure result
  let
    failures = Array.length (Array.filter isLeft results)
  liftEffect do
    log ("\n" <> show (Array.length tests - failures) <> " passed, " <> show failures <> " failed.")
    when (failures > 0) (setExitCode 1)

assertTrue :: String -> Boolean -> Aff Unit
assertTrue label condition = unless condition (liftEffect (throw label))

assertEqual :: forall value. Eq value => Show value => String -> value -> value -> Aff Unit
assertEqual label expected actual =
  if expected == actual
  then pure unit
  else liftEffect
    (throw (label <> "\n  expected: " <> show expected <> "\n  actual:   " <> show actual))

assertRight :: forall value. String -> Either String value -> Aff Unit
assertRight label result = case result of
  Left errorMessage -> liftEffect (throw (label <> "\n  " <> errorMessage))
  Right _ -> pure unit
