module Main where

import HSBlog qualified
import OptParse
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HSBlog.convertDirectory input output
    ConvertSingle input output -> do
      (title, inputHandle) <- case input of
        Stdin -> pure ("", stdin)
        InputFile file -> (,) file <$> openFile file ReadMode

      outputHandle <- case output of
        Stdout -> pure stdout
        OutputFile file -> do
          exists <- doesFileExist file
          shouldOpenFile <-
            if exists
              then do
                putStrLn "Output file already exists."
                confirm
              else pure True

          if shouldOpenFile then openFile file WriteMode else exitFailure

      HSBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

-- Utils

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  input <- getLine
  case input of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use y or n"
      confirm
