module HSBlog (process, convertSingle) where

import HSBlog.Convert (convert)
import HSBlog.Env (defaultEnv)
import HSBlog.Html qualified as Html
import HSBlog.Markup qualified as Markup
import System.IO

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: String -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse
