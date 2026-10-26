module Foldkit.Query
  ( Locator
  , RoleOptions
  , defaultRoleOptions
  , role
  , namedRole
  , roleWith
  , label
  , text
  , textContaining
  , placeholder
  , altText
  , title
  , testId
  , displayValue
  , within
  , first
  , last
  , nth
  , describe
  , NodeRef
  , find
  , findAll
  , findRef
  , targetHtml
  , findClickHandler
  , findInputHandler
  , findChangeHandler
  , findCheckHandler
  , findKeyDownHandler
  , findSubmitHandler
  , isSubmitButton
  , isDisabled
  , isChecked
  , textContent
  , normalizeWhitespace
  , attributeValue
  , hasAttribute
  , booleanAttribute
  , classList
  , accessibleName
  , accessibleDescription
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (all, any, elem, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Foldkit.Html (Attribute(..), Html(..), KeyEvent)
import Foldkit.Html as Html

type RoleOptions =
  { name :: Maybe String, level :: Maybe Int, disabled :: Maybe Boolean, checked :: Maybe Boolean }

defaultRoleOptions :: RoleOptions
defaultRoleOptions = { name: Nothing, level: Nothing, disabled: Nothing, checked: Nothing }

data Locator
  = ByRole String RoleOptions
  | ByLabel String
  | ByText String Boolean
  | ByPlaceholder String
  | ByAltText String
  | ByTitle String
  | ByTestId String
  | ByDisplayValue String
  | Within Locator Locator
  | First Locator
  | Last Locator
  | Nth Int Locator

role :: String -> Locator
role value = ByRole value defaultRoleOptions

namedRole :: String -> String -> Locator
namedRole value nameValue = ByRole value defaultRoleOptions { name = Just nameValue }

roleWith :: String -> RoleOptions -> Locator
roleWith = ByRole

label :: String -> Locator
label = ByLabel

text :: String -> Locator
text value = ByText value true

textContaining :: String -> Locator
textContaining value = ByText value false

placeholder :: String -> Locator
placeholder = ByPlaceholder

altText :: String -> Locator
altText = ByAltText

title :: String -> Locator
title = ByTitle

testId :: String -> Locator
testId = ByTestId

displayValue :: String -> Locator
displayValue = ByDisplayValue

within :: Locator -> Locator -> Locator
within parent child = Within parent child

first :: Locator -> Locator
first = First

last :: Locator -> Locator
last = Last

nth :: Int -> Locator -> Locator
nth = Nth

describe :: Locator -> String
describe = case _ of
  ByRole value options -> "role "
    <> value
    <> ( case options.name of
           Nothing -> ""
           Just nameValue -> " named \"" <> nameValue <> "\""
       )
  ByLabel value -> "label \"" <> value <> "\""
  ByText value true -> "text \"" <> value <> "\""
  ByText value false -> "text containing \"" <> value <> "\""
  ByPlaceholder value -> "placeholder \"" <> value <> "\""
  ByAltText value -> "alt text \"" <> value <> "\""
  ByTitle value -> "title \"" <> value <> "\""
  ByTestId value -> "test id \"" <> value <> "\""
  ByDisplayValue value -> "display value \"" <> value <> "\""
  Within parent child -> describe child <> " within " <> describe parent
  First value -> "first " <> describe value
  Last value -> "last " <> describe value
  Nth index value -> "index " <> show index <> " of " <> describe value

type NodeRef message =
  { path :: Array Int, element :: Html.Element message, ancestors :: Array (Html.Element message) }

refs :: forall message. Html message -> Array (NodeRef message)
refs = go [] []
  where
  go path ancestors node = case node of
    Element element -> [ { path, element, ancestors } ]
      <> Array.concatMap
           (\(Tuple index child) -> go (path <> [ index ]) (ancestors <> [ element ]) child)
           (Array.mapWithIndex Tuple element.children)
    Fragment children ->
      Array.concatMap (\(Tuple index child) -> go (path <> [ index ]) ancestors child)
        (Array.mapWithIndex Tuple children)
    _ -> []

isPrefix :: Array Int -> Array Int -> Boolean
isPrefix prefix value =
  Array.length prefix <= Array.length value && Array.take (Array.length prefix) value == prefix

deepest :: forall message. Array (NodeRef message) -> Array (NodeRef message)
deepest candidates = Array.filter
  ( \candidate -> not
      ( any (\other -> candidate.path /= other.path && isPrefix candidate.path other.path)
          candidates
      )
  )
  candidates

refsFor :: forall message. Locator -> Html message -> Array (NodeRef message)
refsFor locator root = case locator of
  ByRole value options -> Array.filter (matchesRole value options root) (refs root)
  ByLabel value -> Array.filter (matchesLabel value root) (refs root)
  ByText value exact -> deepest (Array.filter (matchesText value exact) (refs root))
  ByPlaceholder value -> Array.filter (matchesAttribute "placeholder" value) (refs root)
  ByAltText value -> Array.filter (matchesAttribute "alt" value) (refs root)
  ByTitle value -> Array.filter (matchesAttribute "title" value) (refs root)
  ByTestId value -> Array.filter (matchesAttribute "data-testid" value) (refs root)
  ByDisplayValue value -> Array.filter (matchesDisplayValue value) (refs root)
  Within parent child -> case Array.head (refsFor parent root) of
    Nothing -> []
    Just parentRef -> refsFor child (Element parentRef.element)
  First value -> Array.take 1 (refsFor value root)
  Last value -> case Array.last (refsFor value root) of
    Nothing -> []
    Just valueRef -> [ valueRef ]
  Nth index value -> case Array.index (refsFor value root) index of
    Nothing -> []
    Just valueRef -> [ valueRef ]

findRef :: forall message. Locator -> Html message -> Maybe (NodeRef message)
findRef locator root = Array.head (refsFor locator root)

targetHtml :: forall message. NodeRef message -> Html message
targetHtml ref = Element ref.element

handlerOn :: forall message value. (Attribute message -> Maybe value) -> Html message -> Maybe value
handlerOn extract = case _ of
  Element element -> Array.findMap extract element.attributes
  _ -> Nothing

findInPath ::
  forall message value. (Attribute message -> Maybe value) -> NodeRef message -> Maybe value
findInPath extract ref = case handlerOn extract (targetHtml ref) of
  Just value -> Just value
  Nothing -> Array.findMap (handlerOn extract <<< Element) (Array.reverse ref.ancestors)

findClickHandler :: forall message. NodeRef message -> Maybe message
findClickHandler = findInPath case _ of
  OnClick message -> Just message
  _ -> Nothing

findInputHandler :: forall message. NodeRef message -> Maybe (String -> message)
findInputHandler = findInPath case _ of
  OnInput handler -> Just handler
  _ -> Nothing

findChangeHandler :: forall message. NodeRef message -> Maybe (String -> message)
findChangeHandler = findInPath case _ of
  OnChange handler -> Just handler
  _ -> Nothing

findCheckHandler :: forall message. NodeRef message -> Maybe (Boolean -> message)
findCheckHandler = findInPath case _ of
  OnCheck handler -> Just handler
  _ -> Nothing

findKeyDownHandler :: forall message. NodeRef message -> Maybe (KeyEvent -> message)
findKeyDownHandler = findInPath case _ of
  OnKeyDown handler -> Just handler
  _ -> Nothing

findSubmitHandler :: forall message. NodeRef message -> Maybe message
findSubmitHandler ref = case formHandler (targetHtml ref) of
  Just message -> Just message
  Nothing -> Array.findMap (formHandler <<< Element) (Array.reverse ref.ancestors)
  where
  formHandler = case _ of
    Element element
      | element.tag == "form" -> handlerOn submitHandler (Element element)
    _ -> Nothing
  submitHandler = case _ of
    OnSubmit message -> Just message
    _ -> Nothing

isSubmitButton :: forall message. NodeRef message -> Boolean
isSubmitButton ref = any isSubmitButtonElement (Array.snoc ref.ancestors ref.element)

isSubmitButtonElement :: forall message. Html.Element message -> Boolean
isSubmitButtonElement element = case element.tag of
  "button" -> case Str.toLower (fromMaybe "submit" (lookupAttribute "type" element.attributes)) of
    "submit" -> true
    _ -> false
  "input" -> Str.toLower (fromMaybe "" (lookupAttribute "type" element.attributes)) == "submit"
  _ -> false

find :: forall message. Locator -> Html message -> Maybe (Html message)
find locator root = map (Element <<< _.element) (Array.head (refsFor locator root))

findAll :: forall message. Locator -> Html message -> Array (Html message)
findAll locator root = map (Element <<< _.element) (refsFor locator root)

textContent :: forall message. Html message -> String
textContent = case _ of
  Text value -> value
  Element element -> Array.foldMap textContent element.children
  Fragment children -> Array.foldMap textContent children
  Empty -> ""

attributeValue :: forall message. String -> Html message -> Maybe String
attributeValue key = case _ of
  Element element -> lookupAttribute key element.attributes
  _ -> Nothing

hasAttribute :: forall message. String -> Html message -> Boolean
hasAttribute key = case _ of
  Element element -> hasAttributeElement key element
  _ -> false

booleanAttribute :: forall message. String -> Html message -> Maybe Boolean
booleanAttribute key = case _ of
  Element element -> booleanAttributeElement key element
  _ -> Nothing

classList :: forall message. Html message -> Array String
classList html = Array.filter (not <<< Str.null)
  (Str.split (Str.Pattern " ") (fromMaybe "" (attributeValue "class" html)))

lookupAttribute :: forall message. String -> Array (Attribute message) -> Maybe String
lookupAttribute key = Array.findMap case _ of
  Attribute name value
    | name == key -> Just value
  BooleanAttribute name true
    | name == key -> Just "true"
  _ -> Nothing

hasAttributeElement :: forall message. String -> Html.Element message -> Boolean
hasAttributeElement key element =
  isJust (lookupAttribute key element.attributes) || isJust (booleanAttributeElement key element)

booleanAttributeElement :: forall message. String -> Html.Element message -> Maybe Boolean
booleanAttributeElement key element = Array.findMap
  ( case _ of
      BooleanAttribute name value
        | name == key -> Just value
      _ -> Nothing
  )
  element.attributes

normalizeWhitespace :: String -> String
normalizeWhitespace = Str.trim
  <<< intercalate " "
  <<< Array.filter (not <<< Str.null)
  <<< Str.split (Str.Pattern " ")
  <<< Str.replaceAll (Str.Pattern "\n") (Str.Replacement " ")
  <<< Str.replaceAll (Str.Pattern "\t") (Str.Replacement " ")
  <<< Str.replaceAll (Str.Pattern "\r") (Str.Replacement " ")

matchesText :: forall message. String -> Boolean -> NodeRef message -> Boolean
matchesText target exact ref =
  let
    actual = normalizeWhitespace (textContent (Element ref.element))
    expected = normalizeWhitespace target
  in
    if exact then actual == expected else Str.contains (Str.Pattern expected) actual

matchesAttribute :: forall message. String -> String -> NodeRef message -> Boolean
matchesAttribute key target ref = lookupAttribute key ref.element.attributes == Just target

matchesDisplayValue :: forall message. String -> NodeRef message -> Boolean
matchesDisplayValue target ref =
  isFormControl ref.element && lookupAttribute "value" ref.element.attributes == Just target

isFormControl :: forall message. Html.Element message -> Boolean
isFormControl element = element.tag `elem` [ "input", "textarea", "select" ]

isDisabled :: forall message. Html.Element message -> Boolean
isDisabled element = booleanAttributeElement "disabled" element
  == Just true
  || lookupAttribute "aria-disabled" element.attributes
  == Just "true"

isChecked :: forall message. Html.Element message -> Boolean
isChecked element = booleanAttributeElement "checked" element
  == Just true
  || lookupAttribute "aria-checked" element.attributes
  == Just "true"

isLabelable :: forall message. Html.Element message -> Boolean
isLabelable element =
  element.tag `elem` [ "button", "input", "meter", "output", "progress", "select", "textarea" ]

hasAriaLabel :: forall message. Html.Element message -> Boolean
hasAriaLabel element = isJust (lookupAttribute "aria-label" element.attributes)
  || isJust (lookupAttribute "aria-labelledby" element.attributes)

matchesLabel :: forall message. String -> Html message -> NodeRef message -> Boolean
matchesLabel target root ref = (isLabelable ref.element || hasAriaLabel ref.element)
  && normalizeWhitespace (accessibleNameElement root ref.element)
  == normalizeWhitespace target

matchesRole :: forall message. String -> RoleOptions -> Html message -> NodeRef message -> Boolean
matchesRole target options root ref =
  let
    element = ref.element
    explicitRoles = lookupAttribute "role" element.attributes
    roles = case explicitRoles of
      Just value -> Str.split (Str.Pattern " ") value
      Nothing -> implicitRoles element
  in
    target `elem` roles && optionsMatch options root element

optionsMatch :: forall message. RoleOptions -> Html message -> Html.Element message -> Boolean
optionsMatch options root element = nameMatches && levelMatches && disabledMatches && checkedMatches
  where
  nameMatches = case options.name of
    Nothing -> true
    Just expected ->
      normalizeWhitespace (accessibleNameElement root element) == normalizeWhitespace expected
  levelMatches = case options.level of
    Nothing -> true
    Just expected -> headingLevel element == Just expected
  disabledMatches = case options.disabled of
    Nothing -> true
    Just expected -> isDisabled element == expected
  checkedMatches = case options.checked of
    Nothing -> true
    Just expected -> isChecked element == expected

headingLevel :: forall message. Html.Element message -> Maybe Int
headingLevel element = case element.tag of
  "h1" -> Just 1
  "h2" -> Just 2
  "h3" -> Just 3
  "h4" -> Just 4
  "h5" -> Just 5
  "h6" -> Just 6
  _ -> Nothing

isDisabled :: forall message. Html.Element message -> Boolean
isDisabled element = booleanAttributeElement "disabled" element
  == Just true
  || lookupAttribute "aria-disabled" element.attributes
  == Just "true"

isChecked :: forall message. Html.Element message -> Boolean
isChecked element = booleanAttributeElement "checked" element
  == Just true
  || lookupAttribute "aria-checked" element.attributes
  == Just "true"

implicitRoles :: forall message. Html.Element message -> Array String
implicitRoles element = case element.tag of
  "a" -> if hasAttributeElement "href" element then [ "link" ] else []
  "article" -> [ "article" ]
  "aside" -> [ "complementary" ]
  "button" -> [ "button" ]
  "dialog" -> [ "dialog" ]
  "fieldset" -> [ "group" ]
  "figure" -> [ "figure" ]
  "footer" -> [ "contentinfo" ]
  "form" -> [ "form" ]
  "h1" -> [ "heading" ]
  "h2" -> [ "heading" ]
  "h3" -> [ "heading" ]
  "h4" -> [ "heading" ]
  "h5" -> [ "heading" ]
  "h6" -> [ "heading" ]
  "header" -> [ "banner" ]
  "hr" -> [ "separator" ]
  "img" -> if isJust (lookupAttribute "alt" element.attributes) then [ "img" ] else []
  "input" -> case Str.toLower (fromMaybe "text" (lookupAttribute "type" element.attributes)) of
    "button" -> [ "button" ]
    "checkbox" -> [ "checkbox" ]
    "email" -> [ "textbox" ]
    "hidden" -> []
    "image" -> [ "button" ]
    "number" -> [ "spinbutton" ]
    "radio" -> [ "radio" ]
    "range" -> [ "slider" ]
    "reset" -> [ "button" ]
    "search" -> [ "searchbox" ]
    "submit" -> [ "button" ]
    "tel" -> [ "textbox" ]
    "text" -> [ "textbox" ]
    "url" -> [ "textbox" ]
    _ -> [ "textbox" ]
  "li" -> [ "listitem" ]
  "main" -> [ "main" ]
  "nav" -> [ "navigation" ]
  "ol" -> [ "list" ]
  "option" -> [ "option" ]
  "p" -> [ "paragraph" ]
  "progress" -> [ "progressbar" ]
  "section" ->
    if isJust (lookupAttribute "aria-label" element.attributes)
      || isJust (lookupAttribute "aria-labelledby" element.attributes)
    then [ "region" ]
    else []
  "select" -> if hasAttributeElement "multiple" element then [ "listbox" ] else [ "combobox" ]
  "summary" -> [ "button" ]
  "table" -> [ "table" ]
  "tbody" -> [ "rowgroup" ]
  "td" -> [ "cell" ]
  "textarea" -> [ "textbox" ]
  "th" -> [ "columnheader" ]
  "thead" -> [ "rowgroup" ]
  "tr" -> [ "row" ]
  "ul" -> [ "list" ]
  _ -> []

accessibleName :: forall message. Html message -> Html message -> String
accessibleName root = case _ of
  Element element -> accessibleNameElement root element
  _ -> ""

accessibleNameElement :: forall message. Html message -> Html.Element message -> String
accessibleNameElement root element = fromMaybe ""
  $ nameFromLabelledBy root element
  <|> nameFromAriaLabel element
  <|> nameFromLabelFor root element
  <|> nameFromNative element
  <|> nameFromContent element
  <|> nameFromTitle element

accessibleDescription :: forall message. Html message -> Html message -> String
accessibleDescription root = case _ of
  Element element -> fromMaybe ""
    $ nameFromDescribedBy root element
    <|> lookupAttribute "aria-description" element.attributes
  _ -> ""

nameFromLabelledBy :: forall message. Html message -> Html.Element message -> Maybe String
nameFromLabelledBy root element = do
  ids <- lookupAttribute "aria-labelledby" element.attributes
  let
    names = Array.mapMaybe (findTextById root) (Str.split (Str.Pattern " ") ids)
  if Array.null names then Nothing else Just (intercalate " " (map normalizeWhitespace names))

nameFromAriaLabel :: forall message. Html.Element message -> Maybe String
nameFromAriaLabel element = do
  value <- lookupAttribute "aria-label" element.attributes
  if Str.null (Str.trim value) then Nothing else Just value

nameFromLabelFor :: forall message. Html message -> Html.Element message -> Maybe String
nameFromLabelFor root element = do
  id <- lookupAttribute "id" element.attributes
  findLabelForId root id

nameFromNative :: forall message. Html.Element message -> Maybe String
nameFromNative element = case element.tag of
  "img" -> lookupAttribute "alt" element.attributes
  "input" -> case Str.toLower (fromMaybe "text" (lookupAttribute "type" element.attributes)) of
    "submit" -> lookupAttribute "value" element.attributes
    "button" -> lookupAttribute "value" element.attributes
    "reset" -> lookupAttribute "value" element.attributes
    "image" -> lookupAttribute "alt" element.attributes
    _ -> Nothing
  _ -> Nothing

nameFromContent :: forall message. Html.Element message -> Maybe String
nameFromContent element =
  if canNameFromContent element
  then
    let
      value = normalizeWhitespace (textContent (Element element))
    in
      if Str.null value then Nothing else Just value
  else Nothing

canNameFromContent :: forall message. Html.Element message -> Boolean
canNameFromContent element =
  element.tag `elem` [ "a", "button", "h1", "h2", "h3", "h4", "h5", "h6", "option", "summary" ]
    || case lookupAttribute "role" element.attributes of
      Nothing -> false
      Just roleValue -> roleValue
        `elem` [ "button"
               , "checkbox"
               , "heading"
               , "link"
               , "menuitem"
               , "option"
               , "radio"
               , "switch"
               , "tab"
               , "treeitem"
               ]

nameFromTitle :: forall message. Html.Element message -> Maybe String
nameFromTitle element = lookupAttribute "title" element.attributes

nameFromDescribedBy :: forall message. Html message -> Html.Element message -> Maybe String
nameFromDescribedBy root element = do
  ids <- lookupAttribute "aria-describedby" element.attributes
  let
    names = Array.mapMaybe (findTextById root) (Str.split (Str.Pattern " ") ids)
  if Array.null names then Nothing else Just (intercalate " " (map normalizeWhitespace names))

findTextById :: forall message. Html message -> String -> Maybe String
findTextById root id = map (textContent <<< Element <<< _.element)
  (Array.find (\ref -> lookupAttribute "id" ref.element.attributes == Just id) (refs root))

findLabelForId :: forall message. Html message -> String -> Maybe String
findLabelForId root id =
  let
    labels = Array.filter (\ref -> ref.element.tag == "label") (refs root)
  in
    Array.findMap
      ( \ref ->
          let
            isFor = lookupAttribute "for" ref.element.attributes == Just id
            containsId = any (\child -> lookupAttribute "id" child.element.attributes == Just id)
              (refs (Element ref.element))
          in
            if isFor || containsId
            then Just (normalizeWhitespace (textContent (Element ref.element)))
            else Nothing
      )
      labels
