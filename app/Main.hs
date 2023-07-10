module Main where

import GHC.IO
import HSBlog (convertSingle)
import HSBlog.Directory (confirm, convertDirectory)
import OptParse
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      convertDirectory input output
    ConvertSingle input output -> do
      let withInputHandle :: (String -> Handle -> IO a) -> IO a
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
                    then confirm "File exists, are you sure you want to overwrite?"
                    else pure True
                if shouldOpenFile
                  then bracket (openFile file WriteMode) hClose action
                  else exitFailure
       in withInputHandle
            ( \title ->
                withOutputHandle . convertSingle title
            )
