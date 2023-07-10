module Main where

import GHC.IO
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
      let
        withInputHandle :: (String -> Handle -> IO a) -> IO a
        withInputHandle action =
          case input of
            Stdin -> action "" stdin
            InputFile file ->
              bracket
                (openFile file ReadMode)
                hClose
                (action file)

        withOutputHandle :: (Handle -> IO a) -> IO a
        withOutputHandle action =
          case output of
            Stdout -> action stdout
            OutputFile file -> do
              exists <- doesFileExist file
              shouldOpenFile <-
                if exists
                  then confirm
                  else pure True
              if shouldOpenFile
                then bracket (openFile file WriteMode) hClose action
                else exitFailure
       in
        withInputHandle
          ( \title ->
              withOutputHandle . HSBlog.convertSingle title
          )

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
