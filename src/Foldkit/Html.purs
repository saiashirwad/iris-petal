module Foldkit.Html
  ( Html(..)
  , Element
  , Attribute(..)
  , Event
  , defaultEvent
  , text
  , element
  , fragment
  , empty
  , attr
  , flag
  , prop
  , on
  , onClick
  , onInput
  , onChange
  , onCheck
  , onSubmit
  , onKeyDown
  , onFocus
  , onBlur
  , class_
  , id_
  , for_
  , name_
  , type_
  , value_
  , checked_
  , disabled_
  , placeholder_
  , href_
  , role_
  , title_
  , alt_
  , src_
  , ariaLabel
  , ariaDescribedBy
  , ariaBusy
  , ariaChecked
  , testId
  , dataAttribute
  , div
  , span
  , p
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , button
  , form
  , label
  , input
  , textarea
  , select
  , option
  , ul
  , ol
  , li
  , article
  , section
  , header
  , footer
  , main
  , nav
  , aside
  , a
  , strong
  , em
  , small
  , code
  , pre
  , br
  , hr
  , img
  , figure
  , figcaption
  , table
  , thead
  , tbody
  , tr
  , th
  , td
  , fieldset
  , legend
  , dialog
  , details
  , summary
  ) where

import Prelude

type Event =
  { value :: String
  , checked :: Boolean
  , key :: String
  , shiftKey :: Boolean
  , ctrlKey :: Boolean
  , altKey :: Boolean
  , metaKey :: Boolean
  }

defaultEvent :: Event
defaultEvent =
  { value: ""
  , checked: false
  , key: ""
  , shiftKey: false
  , ctrlKey: false
  , altKey: false
  , metaKey: false
  }

type Element message =
  { tag :: String
  , attributes :: Array (Attribute message)
  , children :: Array (Html message)
  }

data Html message
  = Element (Element message)
  | Text String
  | Fragment (Array (Html message))
  | Empty

data Attribute message
  = Attribute String String
  | BooleanAttribute String Boolean
  | Property String String
  | Event String (Event -> message)

instance functorHtml :: Functor Html where
  map f = case _ of
    Element value ->
      Element
        { tag: value.tag
        , attributes: map (mapAttribute f) value.attributes
        , children: map (map f) value.children
        }
    Text value -> Text value
    Fragment values -> Fragment (map (map f) values)
    Empty -> Empty

mapAttribute :: forall a b. (a -> b) -> Attribute a -> Attribute b
mapAttribute f = case _ of
  Attribute key value -> Attribute key value
  BooleanAttribute key value -> BooleanAttribute key value
  Property key value -> Property key value
  Event event handler -> Event event (f <<< handler)

text :: forall message. String -> Html message
text = Text

element
  :: forall message
   . String
  -> Array (Attribute message)
  -> Array (Html message)
  -> Html message
element tag attributes children =
  Element { tag, attributes, children }

fragment :: forall message. Array (Html message) -> Html message
fragment = Fragment

empty :: forall message. Html message
empty = Empty

attr :: forall message. String -> String -> Attribute message
attr = Attribute

flag :: forall message. String -> Boolean -> Attribute message
flag = BooleanAttribute

prop :: forall message. String -> String -> Attribute message
prop = Property

on :: forall message. String -> (Event -> message) -> Attribute message
on = Event

onClick :: forall message. message -> Attribute message
onClick message = Event "click" (const message)

onInput :: forall message. (String -> message) -> Attribute message
onInput f = Event "input" (\event -> f event.value)

onChange :: forall message. (String -> message) -> Attribute message
onChange f = Event "change" (\event -> f event.value)

onCheck :: forall message. (Boolean -> message) -> Attribute message
onCheck f = Event "change" (\event -> f event.checked)

onSubmit :: forall message. message -> Attribute message
onSubmit message = Event "submit" (const message)

onKeyDown :: forall message. (Event -> message) -> Attribute message
onKeyDown = Event "keydown"

onFocus :: forall message. message -> Attribute message
onFocus message = Event "focus" (const message)

onBlur :: forall message. message -> Attribute message
onBlur message = Event "blur" (const message)

class_ :: forall message. String -> Attribute message
class_ = Attribute "class"

id_ :: forall message. String -> Attribute message
id_ = Attribute "id"

for_ :: forall message. String -> Attribute message
for_ = Attribute "for"

name_ :: forall message. String -> Attribute message
name_ = Attribute "name"

type_ :: forall message. String -> Attribute message
type_ = Attribute "type"

value_ :: forall message. String -> Attribute message
value_ = Attribute "value"

checked_ :: forall message. Boolean -> Attribute message
checked_ = BooleanAttribute "checked"

disabled_ :: forall message. Boolean -> Attribute message
disabled_ = BooleanAttribute "disabled"

placeholder_ :: forall message. String -> Attribute message
placeholder_ = Attribute "placeholder"

href_ :: forall message. String -> Attribute message
href_ = Attribute "href"

role_ :: forall message. String -> Attribute message
role_ = Attribute "role"

title_ :: forall message. String -> Attribute message
title_ = Attribute "title"

alt_ :: forall message. String -> Attribute message
alt_ = Attribute "alt"

src_ :: forall message. String -> Attribute message
src_ = Attribute "src"

ariaLabel :: forall message. String -> Attribute message
ariaLabel = Attribute "aria-label"

ariaDescribedBy :: forall message. String -> Attribute message
ariaDescribedBy = Attribute "aria-describedby"

ariaBusy :: forall message. Boolean -> Attribute message
ariaBusy value = Attribute "aria-busy" (if value then "true" else "false")

ariaChecked :: forall message. Boolean -> Attribute message
ariaChecked value = Attribute "aria-checked" (if value then "true" else "false")

testId :: forall message. String -> Attribute message
testId = Attribute "data-testid"

dataAttribute :: forall message. String -> String -> Attribute message
dataAttribute key = Attribute ("data-" <> key)

div :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
div = element "div"

span :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
span = element "span"

p :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
p = element "p"

h1 :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
h1 = element "h1"

h2 :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
h2 = element "h2"

h3 :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
h3 = element "h3"

h4 :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
h4 = element "h4"

h5 :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
h5 = element "h5"

h6 :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
h6 = element "h6"

button :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
button = element "button"

form :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
form = element "form"

label :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
label = element "label"

input :: forall message. Array (Attribute message) -> Html message
input attributes = element "input" attributes []

textarea :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
textarea = element "textarea"

select :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
select = element "select"

option :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
option = element "option"

ul :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
ul = element "ul"

ol :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
ol = element "ol"

li :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
li = element "li"

article :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
article = element "article"

section :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
section = element "section"

header :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
header = element "header"

footer :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
footer = element "footer"

main :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
main = element "main"

nav :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
nav = element "nav"

aside :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
aside = element "aside"

a :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
a = element "a"

strong :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
strong = element "strong"

em :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
em = element "em"

small :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
small = element "small"

code :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
code = element "code"

pre :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
pre = element "pre"

br :: forall message. Array (Attribute message) -> Html message
br attributes = element "br" attributes []

hr :: forall message. Array (Attribute message) -> Html message
hr attributes = element "hr" attributes []

img :: forall message. Array (Attribute message) -> Html message
img attributes = element "img" attributes []

figure :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
figure = element "figure"

figcaption :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
figcaption = element "figcaption"

table :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
table = element "table"

thead :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
thead = element "thead"

tbody :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
tbody = element "tbody"

tr :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
tr = element "tr"

th :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
th = element "th"

td :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
td = element "td"

fieldset :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
fieldset = element "fieldset"

legend :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
legend = element "legend"

dialog :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
dialog = element "dialog"

details :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
details = element "details"

summary :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
summary = element "summary"
