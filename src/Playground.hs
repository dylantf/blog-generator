{-# LANGUAGE LambdaCase #-}

module Playground () where

import Control.Exception (Exception, catch, throwIO)
import System.IO (hPutStrLn, stderr)

data ParseDigitError where
  NotADigit :: Char -> ParseDigitError
  deriving (Show)

parseDigit :: Char -> Either ParseDigitError Int
parseDigit c =
  case c of
    '0' -> Right 0
    '1' -> Right 1
    '2' -> Right 2
    '3' -> Right 3
    '4' -> Right 4
    '5' -> Right 5
    '6' -> Right 6
    '7' -> Right 7
    '8' -> Right 8
    '9' -> Right 9
    _ -> Left (NotADigit c)

max3Chars :: Char -> Char -> Char -> Either ParseDigitError Int
max3Chars x y z =
  (\a b c -> max c (max a b))
    <$> parseDigit x
    <*> parseDigit y
    <*> parseDigit z

data MyException
  = ErrZero
  | ErrOdd Int
  deriving (Show)

instance Exception MyException

sayDiv2 :: Int -> IO ()
sayDiv2 n
  | n == 0 = throwIO ErrZero
  | odd n = throwIO (ErrOdd n)
  | otherwise = print (n `div` 2)

main :: IO ()
main =
  catch
    ( do
        putStrLn "Going to print a number now."
        sayDiv2 7
        putStrLn "Did you like it?"
    )
    ( \case
        ErrZero ->
          hPutStrLn stderr "Error: We don't support diving by zero"
        ErrOdd n ->
          hPutStrLn stderr ("Error: " <> show n <> " is odd and can't be evenly divided")
    )
