-- | Accessible queries over the pure `Html` tree: what a user perceives, not
-- | how the view is built. Roles are `Petal.Html.Role` constructors, shared
-- | with the `role_` attribute.
-- |
-- | Builders (`role`, `named`, `label`, ...) are for tests. The query half
-- | (`findAll`, `accessibleName`, ...) is what `Petal.Test` interprets them
-- | with.
module Petal.Locator
  ( Locator
  , role
  , named
  , label
  , text
  , textContaining
  , placeholder
  , altText
  , displayValue
  , within
  , nth
  , describe
  , Match
  , findAll
  , textOf
  , attribute
  , isDisabled
  , isChecked
  , accessibleName
  , accessibleDescription
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (any, elem, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as Str
import Petal.Html (Attribute(..), Element, Html(..), Role(..), roleName)

data Locator
  = ByRole Role (Maybe String)
  | ByLabel String
  | ByText TextMatch
  | ByPlaceholder String
  | ByAltText String
  | ByDisplayValue String
  | Within Locator Locator
  | Nth Int Locator

data TextMatch = Exactly String | Containing String

role :: Role -> Locator
role value = ByRole value Nothing

-- | A role with an accessible name: `named Button "Charmander"`.
named :: Role -> String -> Locator
named value name = ByRole value (Just name)

label :: String -> Locator
label = ByLabel

text :: String -> Locator
text = ByText <<< Exactly

textContaining :: String -> Locator
textContaining = ByText <<< Containing

placeholder :: String -> Locator
placeholder = ByPlaceholder

altText :: String -> Locator
altText = ByAltText

displayValue :: String -> Locator
displayValue = ByDisplayValue

-- | `child` searched inside the first match of `parent`.
within :: Locator -> Locator -> Locator
within = Within

nth :: Int -> Locator -> Locator
nth = Nth

describe :: Locator -> String
describe = case _ of
  ByRole value Nothing -> "role " <> roleName value
  ByRole value (Just name) -> "role " <> roleName value <> " named \"" <> name <> "\""
  ByLabel value -> "label \"" <> value <> "\""
  ByText (Exactly value) -> "text \"" <> value <> "\""
  ByText (Containing value) -> "text containing \"" <> value <> "\""
  ByPlaceholder value -> "placeholder \"" <> value <> "\""
  ByAltText value -> "alt text \"" <> value <> "\""
  ByDisplayValue value -> "display value \"" <> value <> "\""
  Within parent child -> describe child <> " within " <> describe parent
  Nth index value -> "match " <> show index <> " of " <> describe value

-- | A located element with its ancestors, nearest last. Interactions walk the
-- | ancestors to find a handler, as a DOM event would bubble.
type Match message = { element :: Element message, ancestors :: Array (Element message) }

findAll :: forall message. Locator -> Html message -> Array (Match message)
findAll locator root = case locator of
  ByRole value name -> Array.filter (matchesRole value name root <<< _.element) (matches root)
  ByLabel value -> Array.filter (matchesLabel value root <<< _.element) (matches root)
  ByText match -> innermost (Array.filter (matchesText match <<< _.element) (indexed root))
  ByPlaceholder value ->
    Array.filter (hasAttribute "placeholder" value <<< _.element) (matches root)
  ByAltText value -> Array.filter (hasAttribute "alt" value <<< _.element) (matches root)
  ByDisplayValue value -> Array.filter (matchesDisplayValue value <<< _.element) (matches root)
  Within parent child -> case Array.head (findAll parent root) of
    Nothing -> []
    Just found ->
      map (\inner -> inner { ancestors = found.ancestors <> [ found.element ] <> inner.ancestors })
        (findAll child (Element found.element))
  Nth index value -> maybe [] pure (Array.index (findAll value root) index)

type Indexed message =
  { path :: Array Int, element :: Element message, ancestors :: Array (Element message) }

indexed :: forall message. Html message -> Array (Indexed message)
indexed = go [] []
  where
  go path ancestors = case _ of
    Text _ -> []
    Element element -> [ { path, element, ancestors } ] <> Array.concat
      ( Array.mapWithIndex
          (\index child -> go (Array.snoc path index) (Array.snoc ancestors element) child)
          element.children
      )

matches :: forall message. Html message -> Array (Match message)
matches root = map (\found -> { element: found.element, ancestors: found.ancestors }) (indexed root)

innermost :: forall message. Array (Indexed message) -> Array (Match message)
innermost candidates = map (\found -> { element: found.element, ancestors: found.ancestors })
  ( Array.filter
      ( \candidate -> not
          ( any (\other -> candidate.path /= other.path && isPrefix candidate.path other.path)
              candidates
          )
      )
      candidates
  )

isPrefix :: Array Int -> Array Int -> Boolean
isPrefix prefix value =
  Array.length prefix <= Array.length value && Array.take (Array.length prefix) value == prefix

-- | Text content with whitespace collapsed, as a screen reader reads it.
textOf :: forall message. Element message -> String
textOf element = normalize (rawText (Element element))

rawText :: forall message. Html message -> String
rawText = case _ of
  Text value -> value
  Element element -> Array.foldMap rawText element.children

normalize :: String -> String
normalize = intercalate " "
  <<< Array.filter (not <<< Str.null)
  <<< Str.split (Str.Pattern " ")
  <<< Str.replaceAll (Str.Pattern "\n") (Str.Replacement " ")
  <<< Str.replaceAll (Str.Pattern "\t") (Str.Replacement " ")
  <<< Str.replaceAll (Str.Pattern "\r") (Str.Replacement " ")

attribute :: forall message. String -> Element message -> Maybe String
attribute key element = Array.findMap
  ( case _ of
      Property name value
        | name == key -> Just value
      Flag name true
        | name == key -> Just "true"
      AriaRole value
        | key == "role" -> Just (roleName value)
      _ -> Nothing
  )
  element.attributes

flagValue :: forall message. String -> Element message -> Maybe Boolean
flagValue key element = Array.findMap
  ( case _ of
      Flag name value
        | name == key -> Just value
      _ -> Nothing
  )
  element.attributes

isDisabled :: forall message. Element message -> Boolean
isDisabled element =
  flagValue "disabled" element == Just true || attribute "aria-disabled" element == Just "true"

isChecked :: forall message. Element message -> Boolean
isChecked element =
  flagValue "checked" element == Just true || attribute "aria-checked" element == Just "true"

hasAttribute :: forall message. String -> String -> Element message -> Boolean
hasAttribute key target element = attribute key element == Just target

matchesText :: forall message. TextMatch -> Element message -> Boolean
matchesText match element = case match of
  Exactly target -> textOf element == normalize target
  Containing target -> Str.contains (Str.Pattern (normalize target)) (textOf element)

matchesDisplayValue :: forall message. String -> Element message -> Boolean
matchesDisplayValue target element =
  element.tag `elem` [ "input", "textarea", "select" ] && attribute "value" element == Just target

matchesLabel :: forall message. String -> Html message -> Element message -> Boolean
matchesLabel target root element =
  (isLabelable element || hasAriaLabel element) && accessibleName root element == normalize target

isLabelable :: forall message. Element message -> Boolean
isLabelable element =
  element.tag `elem` [ "button", "input", "meter", "output", "progress", "select", "textarea" ]

hasAriaLabel :: forall message. Element message -> Boolean
hasAriaLabel element =
  isJust (attribute "aria-label" element) || isJust (attribute "aria-labelledby" element)

matchesRole :: forall message. Role -> Maybe String -> Html message -> Element message -> Boolean
matchesRole target name root element = roleOf element
  == Just target
  && maybe true (\wanted -> accessibleName root element == normalize wanted) name

roleOf :: forall message. Element message -> Maybe Role
roleOf element = explicit <|> implicitRole element
  where
  explicit = Array.findMap
    ( case _ of
        AriaRole value -> Just value
        _ -> Nothing
    )
    element.attributes

implicitRole :: forall message. Element message -> Maybe Role
implicitRole element = case element.tag of
  "a" -> if isJust (attribute "href" element) then Just Link else Nothing
  "article" -> Just Article
  "aside" -> Just Complementary
  "button" -> Just Button
  "dialog" -> Just Dialog
  "fieldset" -> Just Group
  "figure" -> Just Figure
  "footer" -> Just ContentInfo
  "form" -> Just Form
  "h1" -> Just Heading
  "h2" -> Just Heading
  "h3" -> Just Heading
  "h4" -> Just Heading
  "h5" -> Just Heading
  "h6" -> Just Heading
  "header" -> Just Banner
  "hr" -> Just Separator
  "img" -> if isJust (attribute "alt" element) then Just Img else Nothing
  "input" -> case Str.toLower (fromMaybe "text" (attribute "type" element)) of
    "button" -> Just Button
    "checkbox" -> Just Checkbox
    "hidden" -> Nothing
    "image" -> Just Button
    "number" -> Just SpinButton
    "radio" -> Just Radio
    "range" -> Just Slider
    "reset" -> Just Button
    "search" -> Just SearchBox
    "submit" -> Just Button
    _ -> Just TextBox
  "li" -> Just ListItem
  "main" -> Just Main
  "nav" -> Just Navigation
  "ol" -> Just List
  "option" -> Just Option
  "p" -> Just Paragraph
  "progress" -> Just ProgressBar
  "section" -> if hasAriaLabel element then Just Region else Nothing
  "select" -> if isJust (attribute "multiple" element) then Just ListBox else Just Combobox
  "summary" -> Just Button
  "table" -> Just Table
  "tbody" -> Just RowGroup
  "td" -> Just Cell
  "textarea" -> Just TextBox
  "th" -> Just ColumnHeader
  "thead" -> Just RowGroup
  "tr" -> Just Row
  "ul" -> Just List
  _ -> Nothing

accessibleName :: forall message. Html message -> Element message -> String
accessibleName root element = normalize
  $ fromMaybe ""
  $ nameFromLabelledBy
  <|> nonBlank (attribute "aria-label" element)
  <|> (attribute "id" element >>= labelFor root)
  <|> nameFromNative
  <|> nameFromContent
  <|> attribute "title" element
  where
  nameFromLabelledBy = attribute "aria-labelledby" element >>= textOfIds root
  nameFromNative = case element.tag of
    "img" -> attribute "alt" element
    "input" -> case Str.toLower (fromMaybe "text" (attribute "type" element)) of
      "submit" -> attribute "value" element
      "button" -> attribute "value" element
      "reset" -> attribute "value" element
      "image" -> attribute "alt" element
      _ -> Nothing
    _ -> Nothing
  nameFromContent =
    if maybe false namedByContent (roleOf element)
    then nonBlank (Just (textOf element))
    else Nothing

accessibleDescription :: forall message. Html message -> Element message -> String
accessibleDescription root element = normalize
  $ fromMaybe ""
  $ (attribute "aria-describedby" element >>= textOfIds root)
  <|> attribute "aria-description" element

namedByContent :: Role -> Boolean
namedByContent = case _ of
  Button -> true
  Cell -> true
  Checkbox -> true
  ColumnHeader -> true
  Heading -> true
  Link -> true
  Option -> true
  Radio -> true
  Switch -> true
  Tab -> true
  _ -> false

nonBlank :: Maybe String -> Maybe String
nonBlank = case _ of
  Just value
    | not (Str.null (Str.trim value)) -> Just value
  _ -> Nothing

textOfIds :: forall message. Html message -> String -> Maybe String
textOfIds root ids =
  let
    texts = Array.mapMaybe (elementById root) (Str.split (Str.Pattern " ") ids)
  in
    if Array.null texts then Nothing else Just (intercalate " " (map textOf texts))

elementById :: forall message. Html message -> String -> Maybe (Element message)
elementById root id =
  _.element <$> Array.find (\found -> attribute "id" found.element == Just id) (matches root)

labelFor :: forall message. Html message -> String -> Maybe String
labelFor root id = Array.findMap
  ( \found ->
      let
        isFor = attribute "for" found.element == Just id
        wraps =
          any (\inner -> attribute "id" inner.element == Just id) (matches (Element found.element))
      in
        if found.element.tag == "label" && (isFor || wraps)
        then Just (textOf found.element)
        else Nothing
  )
  (matches root)
