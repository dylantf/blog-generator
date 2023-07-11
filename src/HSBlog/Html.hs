module HSBlog.Html (
  Html,
  Title,
  Structure,
  empty_,
  html_,
  title_,
  stylesheet_,
  meta_,
  p_,
  h_,
  ul_,
  ol_,
  code_,
  txt_,
  link_,
  img_,
  b_,
  i_,
  render,
)
where

import Numeric.Natural

newtype Html = Html String

newtype Structure = Structure String

newtype Content = Content String

type Title = String

newtype Head = Head String

instance Semigroup Structure where
  (<>) (Structure a) (Structure b) = Structure (a <> b)

instance Monoid Structure where
  mempty = empty_

instance Semigroup Head where
  (<>) (Head a) (Head b) = Head (a <> b)

instance Monoid Head where
  mempty = Head ""

getStructureString :: Structure -> String
getStructureString (Structure s) = s

getContentString :: Content -> String
getContentString (Content c) = c

escape :: String -> String
escape =
  let escapeChar c = case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
   in concatMap escapeChar

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

render :: Html -> String
render (Html html) = html

empty_ :: Structure
empty_ = Structure ""

txt_ :: String -> Content
txt_ = Content . escape

html_ :: Head -> Structure -> Html
html_ (Head headEl) content =
  Html
    ( el
        "html"
        ( el "head" headEl
            <> el "body" (getStructureString content)
        )
    )

-- Head elements

title_ :: String -> Head
title_ = Head . el "title" . escape

stylesheet_ :: FilePath -> Head
stylesheet_ path = Head $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\" />"

meta_ :: String -> String -> Head
meta_ name content =
  Head $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\" />"

-- Body elements

link_ :: FilePath -> Content -> Content
link_ path (Content content) =
  Content $ elAttr "a" ("href=\"" <> escape path <> "\"") content

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\" />"

b_ :: Content -> Content
b_ (Content content) = Content $ el "b" content

i_ :: Content -> Content
i_ (Content content) = Content $ el "i" content

p_ :: Content -> Structure
p_ = Structure . el "p" . escape . getContentString

h_ :: Natural -> Content -> Structure
h_ level = Structure . el ("h" <> show level) . escape . getContentString

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape
