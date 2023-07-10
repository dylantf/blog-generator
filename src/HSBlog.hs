module HSBlog (process, convertSingle) where

import HSBlog.Convert (convert)
import HSBlog.Html qualified as Html
import HSBlog.Markup qualified as Markup
import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
