module HSBlog.Convert (convert) where

import HSBlog.Html qualified as Html
import HSBlog.Markup qualified as Markup

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt -> Html.h_ n $ Html.txt_ txt
    Markup.Paragraph p -> Html.p_ $ Html.txt_ p
    Markup.UnorderedList list -> Html.ul_ $ map (Html.p_ . Html.txt_) list
    Markup.OrderedList list -> Html.ol_ $ map (Html.p_ . Html.txt_) list
    Markup.CodeBlock list -> Html.code_ (unlines list)

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let makePreview (file, doc) =
        case doc of
          Markup.Heading 1 heading : article ->
            Html.h_ 3 (Html.link_ file (Html.txt_ heading))
              <> foldMap convertStructure (take 3 article)
              <> Html.p_ (Html.link_ file (Html.txt_ "..."))
          _ ->
            Html.h_ 3 (Html.link_ file (Html.txt_ file))
      previews = foldMap makePreview files
   in Html.html_
        "Blog"
        ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
            <> Html.h_ 2 (Html.txt_ "Posts")
            <> previews
        )
