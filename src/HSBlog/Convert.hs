module HSBlog.Convert (convert, convertStructure) where

import HSBlog.Env (Env (..))
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

convert :: Env -> String -> Markup.Document -> Html.Html
convert env title doc =
  let headEl =
        Html.title_ (eBlogName env <> " - " <> title)
          <> Html.stylesheet_ (eStylesheetPath env)
      article = foldMap convertStructure doc
      pageTitle = Html.h_ 1 (Html.link_ "index.html" $ Html.txt_ $ eBlogName env)
      body = pageTitle <> article
   in Html.html_ headEl body
