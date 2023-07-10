module HSBlog.Directory (convertDirectory, buildIndex, confirm) where

import Control.Exception (SomeException (..), catch, displayException)
import Control.Monad (void, when)
import Data.List (partition)
import HSBlog.Convert (convert, convertStructure)
import HSBlog.Env (Env (..))
import HSBlog.Html qualified as Html
import HSBlog.Markup qualified as Markup
import System.Directory (copyFile, createDirectory, doesDirectoryExist, listDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, takeExtension, takeFileName, (<.>), (</>))
import System.IO (hPutStrLn, stderr)

buildIndex :: Env -> [(FilePath, Markup.Document)] -> Html.Html
buildIndex env files =
  let makePreview (file, doc) = case doc of
        Markup.Heading 1 heading : article ->
          Html.h_ 3 (Html.link_ file (Html.txt_ heading))
            <> foldMap convertStructure (take 3 article)
            <> Html.p_ (Html.link_ file (Html.txt_ "..."))
        _ ->
          Html.h_ 3 (Html.link_ file (Html.txt_ file))
      previews = foldMap makePreview files
   in Html.html_
        (Html.title_ (eBlogName env) <> Html.stylesheet_ (eStylesheetPath env))
        ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
            <> Html.h_ 2 (Html.txt_ "Posts")
            <> previews
        )

convertDirectory :: Env -> FilePath -> FilePath -> IO ()
convertDirectory env inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let outputFiles = txtsToRenderedHtml env filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputFiles
  putStrLn "Done!"

data DirContents = DirContents
  { dcFilesToProcess :: [(FilePath, String)],
    dcFilesToCopy :: [FilePath]
  }

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (txtFiles, otherFiles) = partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <- applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents {dcFilesToProcess = txtFilesAndContent, dcFilesToCopy = otherFiles}

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs =
  let f input = do
        maybeResult <-
          catch
            (Right <$> action input)
            (\(SomeException e) -> do pure $ Left (displayException e))
        pure (input, maybeResult)
   in traverse f inputs

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled" *> exitFailure)

createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else pure True
  when create (createDirectory dir)
  pure create

-- Perform an action only when inside IO monad
whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  when result action

confirm :: String -> IO Bool
confirm message = do
  putStrLn $ message <> " (y/n)"
  input <- getLine
  case input of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use y or n"
      confirm message

txtsToRenderedHtml :: Env -> [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml env txtFiles =
  let txtOutputFiles = map toOutputMarkupFile txtFiles
      index = ("index.html", buildIndex env txtOutputFiles)
   in map (fmap Html.render) (index : map (convertFile env) txtOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: Env -> (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile env (file, doc) = (file, convert env file doc)

-- Copy files to a directory and log errors to stderr
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

-- Write files to a directory and log errors to stderr
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let writeFileContent (file, content) = writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures
