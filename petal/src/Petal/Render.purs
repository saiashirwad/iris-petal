module Petal.Render (renderToString) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.String as Str
import Petal.Html (Attribute(..), Html(..), roleName)

renderToString :: forall message. Html message -> String
renderToString = case _ of
  Text value -> escapeText value
  Element element -> "<"
    <> element.tag
    <> renderAttributes element.attributes
    <> ">"
    <> ( if isVoid element.tag
         then ""
         else foldMap renderToString element.children <> "</" <> element.tag <> ">"
       )

renderAttributes :: forall message. Array (Attribute message) -> String
renderAttributes = foldMap renderAttribute

renderAttribute :: forall message. Attribute message -> String
renderAttribute = case _ of
  Property name value -> renderAttributeValue name value
  Flag name true -> " " <> name
  Flag _ false -> ""
  AriaRole role -> renderAttributeValue "role" (roleName role)
  Key _ -> ""
  OnClick _ -> ""
  OnInput _ -> ""
  OnChange _ -> ""
  OnCheck _ -> ""
  OnSubmit _ -> ""
  OnKeyDown _ -> ""
  OnFocus _ -> ""
  OnBlur _ -> ""
  OnVisible _ -> ""

renderAttributeValue :: String -> String -> String
renderAttributeValue name value = " " <> name <> "=\"" <> escapeAttribute value <> "\""

isVoid :: String -> Boolean
isVoid tag = Array.elem tag
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
escapeText = Str.replaceAll (Str.Pattern ">") (Str.Replacement "&gt;")
  <<< Str.replaceAll (Str.Pattern "<") (Str.Replacement "&lt;")
  <<< Str.replaceAll (Str.Pattern "&") (Str.Replacement "&amp;")

escapeAttribute :: String -> String
escapeAttribute = Str.replaceAll (Str.Pattern ">") (Str.Replacement "&gt;")
  <<< Str.replaceAll (Str.Pattern "<") (Str.Replacement "&lt;")
  <<< Str.replaceAll (Str.Pattern "\"") (Str.Replacement "&quot;")
  <<< Str.replaceAll (Str.Pattern "&") (Str.Replacement "&amp;")
