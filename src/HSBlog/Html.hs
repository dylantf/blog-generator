module HSBlog.Html (
  Html,
  Title,
  Structure,
  html_,
  empty_,
  p_,
  h_,
  ul_,
  ol_,
  code_,
  -- append_,
  render,
) where

import Numeric.Natural

newtype Html = Html String

newtype Structure = Structure String

type Title = String

getStructureString :: Structure -> String
getStructureString (Structure s) = s

escape :: String -> String
escape =
  let
    escapeChar c = case c of
      '<' -> "&lt;"
      '>' -> "&gt;"
      '&' -> "&amp;"
      '"' -> "&quot;"
      '\'' -> "&#39;"
      _ -> [c]
   in
    concatMap escapeChar

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- Replaced with Semigroup instance definition
-- append_ :: Structure -> Structure -> Structure
-- append_ (Structure a) (Structure b) = Structure (a <> b)

instance Semigroup Structure where
  (<>) (Structure a) (Structure b) = Structure (a <> b)

instance Monoid Structure where
  mempty = empty_

render :: Html -> String
render (Html html) = html

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
  Html (el "head" (el "title" (escape title)) <> el "body" content)

empty_ :: Structure
empty_ = Structure ""

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ level = Structure . el ("h" <> show level) . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

concatStructure :: [Structure] -> Structure
concatStructure list = case list of
  [] -> mempty
  x : xs -> x <> concatStructure xs
