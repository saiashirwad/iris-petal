module Foldkit.Render
  ( Document
  , renderToString
  , renderDocumentToString
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.String as Str
import Foldkit.Html (Attribute(..), Html(..))

type Document message =
  { title :: String
  , body :: Html message
  }

renderToString :: forall message. Html message -> String
renderToString = case _ of
  Text value -> escapeText value
  Empty -> ""
  Fragment children -> foldMap renderToString children
  Element element ->
    "<" <> element.tag <> renderAttributes element.attributes <> ">"
      <> ( if isVoid element.tag then ""
            else foldMap renderToString element.children <> "</" <> element.tag <> ">"
        )

renderDocumentToString :: forall message. Document message -> String
renderDocumentToString document =
  "<!doctype html><html><head><title>"
    <> escapeText document.title
    <> "</title></head><body>"
    <> renderToString document.body
    <> "</body></html>"

renderAttributes :: forall message. Array (Attribute message) -> String
renderAttributes = foldMap renderAttribute

renderAttribute :: forall message. Attribute message -> String
renderAttribute = case _ of
  Attribute name value -> renderAttributeValue name value
  Property name value -> renderAttributeValue name value
  BooleanAttribute name true -> " " <> name
  BooleanAttribute _ false -> ""
  Event _ _ -> ""

renderAttributeValue :: String -> String -> String
renderAttributeValue name value =
  " " <> name <> "=\"" <> escapeAttribute value <> "\""

isVoid :: String -> Boolean
isVoid tag =
  Array.elem tag
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]

escapeText :: String -> String
escapeText =
  Str.replaceAll (Str.Pattern ">") (Str.Replacement "&gt;")
    <<< Str.replaceAll (Str.Pattern "<") (Str.Replacement "&lt;")
    <<< Str.replaceAll (Str.Pattern "&") (Str.Replacement "&amp;")

escapeAttribute :: String -> String
escapeAttribute =
  Str.replaceAll (Str.Pattern ">") (Str.Replacement "&gt;")
    <<< Str.replaceAll (Str.Pattern "<") (Str.Replacement "&lt;")
    <<< Str.replaceAll (Str.Pattern "\"") (Str.Replacement "&quot;")
    <<< Str.replaceAll (Str.Pattern "&") (Str.Replacement "&amp;")
