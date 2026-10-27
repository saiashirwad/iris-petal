-- | A pure view tree. `Html message` is a `Functor`, so a parent embeds a
-- | child view with `map GotChild (Child.view model.child)`.
-- |
-- | There is no fragment and no empty node: an element's children are a plain
-- | array, so conditional content is an array operation and the browser diff
-- | only ever compares elements and text.
module Petal.Html
  ( Html(..)
  , Element
  , Attribute(..)
  , Role(..)
  , KeyEvent
  , roleName
  , text
  , element
  , attr
  , flag
  , key
  , onClick
  , onInput
  , onChange
  , onCheck
  , onSubmit
  , onKeyDown
  , onFocus
  , onBlur
  , onVisible
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
  , loading_
  , ariaLabel
  , ariaDescribedBy
  , ariaBusy
  , ariaChecked
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
  , dl
  , dt
  , dd
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
  )
  where

import Prelude

type KeyEvent =
  { key :: String, shiftKey :: Boolean, ctrlKey :: Boolean, altKey :: Boolean, metaKey :: Boolean }

type Element message =
  { tag :: String, attributes :: Array (Attribute message), children :: Array (Html message) }

data Html message = Element (Element message) | Text String

-- | `Key` identifies an element among its siblings so the browser runtime
-- | moves it instead of patching whatever sits at the same index.
-- | `OnVisible` fires when the element enters the viewport, including at once
-- | if it is already visible when the attribute appears.
data Attribute message
  = Property String String
  | Flag String Boolean
  | AriaRole Role
  | Key String
  | OnClick message
  | OnInput (String -> message)
  | OnChange (String -> message)
  | OnCheck (Boolean -> message)
  | OnSubmit message
  | OnKeyDown (KeyEvent -> message)
  | OnFocus message
  | OnBlur message
  | OnVisible message

-- | WAI-ARIA roles. Views set them with `role_`; locators query them by the
-- | same constructors, so a misspelled role does not compile.
data Role
  = Alert
  | Article
  | Banner
  | Button
  | Cell
  | Checkbox
  | ColumnHeader
  | Combobox
  | Complementary
  | ContentInfo
  | Dialog
  | Figure
  | Form
  | Group
  | Heading
  | Img
  | Link
  | List
  | ListBox
  | ListItem
  | Main
  | Navigation
  | Option
  | Paragraph
  | ProgressBar
  | Radio
  | Region
  | Row
  | RowGroup
  | SearchBox
  | Separator
  | Slider
  | SpinButton
  | Status
  | Switch
  | Tab
  | TabList
  | TabPanel
  | Table
  | TextBox

derive instance eqRole :: Eq Role

instance showRole :: Show Role where
  show = roleName

roleName :: Role -> String
roleName = case _ of
  Alert -> "alert"
  Article -> "article"
  Banner -> "banner"
  Button -> "button"
  Cell -> "cell"
  Checkbox -> "checkbox"
  ColumnHeader -> "columnheader"
  Combobox -> "combobox"
  Complementary -> "complementary"
  ContentInfo -> "contentinfo"
  Dialog -> "dialog"
  Figure -> "figure"
  Form -> "form"
  Group -> "group"
  Heading -> "heading"
  Img -> "img"
  Link -> "link"
  List -> "list"
  ListBox -> "listbox"
  ListItem -> "listitem"
  Main -> "main"
  Navigation -> "navigation"
  Option -> "option"
  Paragraph -> "paragraph"
  ProgressBar -> "progressbar"
  Radio -> "radio"
  Region -> "region"
  Row -> "row"
  RowGroup -> "rowgroup"
  SearchBox -> "searchbox"
  Separator -> "separator"
  Slider -> "slider"
  SpinButton -> "spinbutton"
  Status -> "status"
  Switch -> "switch"
  Tab -> "tab"
  TabList -> "tablist"
  TabPanel -> "tabpanel"
  Table -> "table"
  TextBox -> "textbox"

instance functorHtml :: Functor Html where
  map f = case _ of
    Element value -> Element
      { tag: value.tag
      , attributes: map (mapAttribute f) value.attributes
      , children: map (map f) value.children
      }
    Text value -> Text value

mapAttribute :: forall a b. (a -> b) -> Attribute a -> Attribute b
mapAttribute f = case _ of
  Property name value -> Property name value
  Flag name value -> Flag name value
  AriaRole value -> AriaRole value
  Key value -> Key value
  OnClick message -> OnClick (f message)
  OnInput handler -> OnInput (f <<< handler)
  OnChange handler -> OnChange (f <<< handler)
  OnCheck handler -> OnCheck (f <<< handler)
  OnSubmit message -> OnSubmit (f message)
  OnKeyDown handler -> OnKeyDown (f <<< handler)
  OnFocus message -> OnFocus (f message)
  OnBlur message -> OnBlur (f message)
  OnVisible message -> OnVisible (f message)

text :: forall message. String -> Html message
text = Text

element ::
  forall message. String -> Array (Attribute message) -> Array (Html message) -> Html message
element tag attributes children = Element { tag, attributes, children }

attr :: forall message. String -> String -> Attribute message
attr = Property

flag :: forall message. String -> Boolean -> Attribute message
flag = Flag

key :: forall message. String -> Attribute message
key = Key

onClick :: forall message. message -> Attribute message
onClick = OnClick

onInput :: forall message. (String -> message) -> Attribute message
onInput = OnInput

onChange :: forall message. (String -> message) -> Attribute message
onChange = OnChange

onCheck :: forall message. (Boolean -> message) -> Attribute message
onCheck = OnCheck

onSubmit :: forall message. message -> Attribute message
onSubmit = OnSubmit

onKeyDown :: forall message. (KeyEvent -> message) -> Attribute message
onKeyDown = OnKeyDown

onFocus :: forall message. message -> Attribute message
onFocus = OnFocus

onBlur :: forall message. message -> Attribute message
onBlur = OnBlur

onVisible :: forall message. message -> Attribute message
onVisible = OnVisible

class_ :: forall message. String -> Attribute message
class_ = Property "class"

id_ :: forall message. String -> Attribute message
id_ = Property "id"

for_ :: forall message. String -> Attribute message
for_ = Property "for"

name_ :: forall message. String -> Attribute message
name_ = Property "name"

type_ :: forall message. String -> Attribute message
type_ = Property "type"

value_ :: forall message. String -> Attribute message
value_ = Property "value"

checked_ :: forall message. Boolean -> Attribute message
checked_ = Flag "checked"

disabled_ :: forall message. Boolean -> Attribute message
disabled_ = Flag "disabled"

placeholder_ :: forall message. String -> Attribute message
placeholder_ = Property "placeholder"

href_ :: forall message. String -> Attribute message
href_ = Property "href"

role_ :: forall message. Role -> Attribute message
role_ = AriaRole

title_ :: forall message. String -> Attribute message
title_ = Property "title"

alt_ :: forall message. String -> Attribute message
alt_ = Property "alt"

src_ :: forall message. String -> Attribute message
src_ = Property "src"

loading_ :: forall message. String -> Attribute message
loading_ = Property "loading"

ariaLabel :: forall message. String -> Attribute message
ariaLabel = Property "aria-label"

ariaDescribedBy :: forall message. String -> Attribute message
ariaDescribedBy = Property "aria-describedby"

ariaBusy :: forall message. Boolean -> Attribute message
ariaBusy value = Property "aria-busy" (if value then "true" else "false")

ariaChecked :: forall message. Boolean -> Attribute message
ariaChecked value = Property "aria-checked" (if value then "true" else "false")

dataAttribute :: forall message. String -> String -> Attribute message
dataAttribute key = Property ("data-" <> key)

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

dl :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
dl = element "dl"

dt :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
dt = element "dt"

dd :: forall message. Array (Attribute message) -> Array (Html message) -> Html message
dd = element "dd"

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
