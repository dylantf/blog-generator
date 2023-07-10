module HSBlog (process, convertSingle) where

import HSBlog.Convert (convert)
import HSBlog.Env (Env)
import HSBlog.Html qualified as Html
import HSBlog.Markup qualified as Markup
import System.IO

convertSingle :: Env -> Html.Title -> Handle -> Handle -> IO ()
convertSingle env title input output = do
  content <- hGetContents input
  hPutStrLn output (process env title content)

process :: Env -> Html.Title -> String -> String
process env title = Html.render . convert env title . Markup.parse
