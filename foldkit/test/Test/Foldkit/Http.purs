module Test.Foldkit.Http (spec) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Either (Either(..))
import Foldkit.Http (HttpError(..), readJson)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

decodeHello :: Json -> Either JsonDecodeError String
decodeHello json = do
  record :: { hello :: String } <- decodeJson json
  pure record.hello

spec :: Spec Unit
spec = describe "Http.readJson" do
  it "decodes a 2xx body" do
    readJson decodeHello { status: 200, body: "{\"hello\":\"world\"}" } `shouldEqual` Right "world"

  it "turns a non-2xx status into BadStatus" do
    readJson decodeHello { status: 404, body: "missing" }
      `shouldEqual` Left (BadStatus 404 "missing")

  it "reports malformed JSON as BadBody" do
    case readJson decodeHello { status: 200, body: "{" } of
      Left (BadBody _) -> pure unit
      other -> show other `shouldEqual` "Left (BadBody _)"
