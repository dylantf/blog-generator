module HSBlog (process, convertSingle, convertDirectory) where

import HSBlog.Convert (convert)
import HSBlog.Html qualified as Html
import HSBlog.Markup qualified as Markup
import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented yet"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
