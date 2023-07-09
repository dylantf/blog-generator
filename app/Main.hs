{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use when" #-}

module Main (main) where

import Convert (convert)
import Html qualified
import Markup qualified
import System.Directory (doesFileExist)
import System.Environment (getArgs)

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

main :: IO ()
main =
  getArgs >>= \args ->
    case args of
      [] -> do
        content <- getContents
        putStrLn $ process "No title" content
      [input, output] -> do
        content <- readFile input
        exists <- doesFileExist output
        let writeResult = writeFile output (process input content)
        if exists
          then whenIO confirm writeResult
          else writeResult
      _ -> putStrLn "Usage: stack run [-- <input-file> <output-file>]"

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()

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
