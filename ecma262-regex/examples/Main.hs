{-# LANGUAGE OverloadedStrings #-}

-- | Example usage of the ECMA262 regex library
module Main where

import Text.Regex.ECMA262
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "=== ECMA262 Regex Library Examples ===\n"

  -- Example 1: Simple pattern matching
  example1

  -- Example 2: Capture groups
  example2

  -- Example 3: Case-insensitive matching
  example3

  -- Example 4: Finding all matches
  example4

  -- Example 5: Unicode support
  example5

  -- Example 6: Advanced patterns
  example6

  putStrLn "\n=== All examples completed successfully ==="

example1 :: IO ()
example1 = do
  putStrLn "Example 1: Simple Pattern Matching"
  putStrLn "-----------------------------------"

  regex <- compileOrFail "hello" []
  result <- match regex "hello world"

  case result of
    Nothing -> putStrLn "No match found"
    Just m -> do
      putStrLn $ "Match found: " ++ show (matchText m)
      putStrLn $ "Position: " ++ show (matchStart m) ++ "-" ++ show (matchEnd m)
  putStrLn ""

example2 :: IO ()
example2 = do
  putStrLn "Example 2: Capture Groups"
  putStrLn "--------------------------"

  regex <- compileOrFail "(\\d{4})-(\\d{2})-(\\d{2})" []
  result <- match regex "Date: 2024-03-15"

  case result of
    Nothing -> putStrLn "No match found"
    Just m -> do
      putStrLn $ "Full match: " ++ show (matchText m)
      let [(_, _, year), (_, _, month), (_, _, day)] = captures m
      putStrLn $ "Year: " ++ show year
      putStrLn $ "Month: " ++ show month
      putStrLn $ "Day: " ++ show day
  putStrLn ""

example3 :: IO ()
example3 = do
  putStrLn "Example 3: Case-Insensitive Matching"
  putStrLn "-------------------------------------"

  regex <- compileOrFail "hello" [IgnoreCase]

  let testStrings = ["HELLO", "Hello", "hello", "HeLLo"]
  mapM_ (\s -> do
    result <- test regex s
    putStrLn $ show s ++ ": " ++ if result then "match" else "no match"
    ) testStrings
  putStrLn ""

example4 :: IO ()
example4 = do
  putStrLn "Example 4: Finding All Matches"
  putStrLn "-------------------------------"

  regex <- compileOrFail "\\b\\w+\\b" []
  matches <- matchAll regex "The quick brown fox jumps"

  putStrLn $ "Found " ++ show (length matches) ++ " words:"
  mapM_ (\m -> putStrLn $ "  - " ++ show (matchText m)) matches
  putStrLn ""

example5 :: IO ()
example5 = do
  putStrLn "Example 5: Unicode Support"
  putStrLn "---------------------------"

  regex <- compileOrFail "\\p{Emoji}+" [Unicode]
  result <- match regex "Hello 😀 World 🎉"

  case result of
    Nothing -> putStrLn "No emoji found"
    Just m -> putStrLn $ "Found emoji at position " ++ show (matchStart m)
  putStrLn ""

example6 :: IO ()
example6 = do
  putStrLn "Example 6: Email Validation"
  putStrLn "----------------------------"

  -- Simplified email regex (not RFC compliant, just for demo)
  regex <- compileOrFail "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" []

  let emails =
        [ "user@example.com"
        , "invalid.email"
        , "test.user+tag@domain.co.uk"
        , "not@an@email"
        ]

  mapM_ (\email -> do
    valid <- test regex email
    putStrLn $ show email ++ ": " ++ (if valid then "valid" else "invalid")
    ) emails
  putStrLn ""

-- Helper function to compile or exit on error
compileOrFail :: BS.ByteString -> [RegexFlag] -> IO Regex
compileOrFail pattern flags = do
  result <- compile pattern flags
  case result of
    Left err -> do
      putStrLn $ "Error compiling pattern: " ++ err
      exitFailure
    Right regex -> return regex
