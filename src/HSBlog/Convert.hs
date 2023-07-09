module HSBlog.Convert (convert) where

import HSBlog.Html qualified as Html
import HSBlog.Markup qualified as Markup

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt -> Html.h_ n txt
    Markup.Paragraph p -> Html.p_ p
    Markup.UnorderedList list -> Html.ul_ $ map Html.p_ list
    Markup.OrderedList list -> Html.ol_ $ map Html.p_ list
    Markup.CodeBlock list -> Html.code_ (unlines list)

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
