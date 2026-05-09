{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Text.Regex.ECMA262
-- Description : ECMAScript 262 (JavaScript) regular expressions for Haskell
-- Copyright   : (c) 2025
-- License     : MIT
-- Maintainer  : your-email@example.com
-- Stability   : experimental
--
-- This module provides support for ECMAScript 262 (JavaScript-compatible)
-- regular expressions in Haskell. It uses FFI bindings to QuickJS's libregexp,
-- which is fully compliant with the ECMAScript 2023 specification.
--
-- Example usage:
--
-- @
-- import Text.Regex.ECMA262
-- import qualified Data.ByteString as BS
--
-- main :: IO ()
-- main = do
--   regex <- compile \"^[a-z]+$\" [IgnoreCase]
--   case regex of
--     Left err -> putStrLn $ \"Compilation error: \" ++ err
--     Right r -> do
--       result <- match r \"Hello\"
--       print result  -- Just (Match {matchStart = 0, matchEnd = 5, captures = []})
-- @
module Text.Regex.ECMA262
  ( -- * Types
    Regex
  , Match(..)
  , I.RegexFlag(..)
    -- * Compilation
  , compile
  , compileText
    -- * Matching
  , match
  , matchAll
  , test
    -- * Advanced
  , matchFrom
  , getCaptureCount
  , getFlags
  , getGroupNames
    -- * Re-exports for convenience
  , BS.ByteString
  , T.Text
  ) where

import qualified Text.Regex.ECMA262.Internal as I
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Exception (bracket)
import Data.Maybe (isJust)
import GHC.Generics (Generic)

-- | A compiled regular expression
data Regex = Regex
  { regexPtr :: ForeignPtr I.Regex
  , regexPattern :: BS.ByteString
  , regexFlags :: [I.RegexFlag]
  }
  deriving (Generic)

instance Show Regex where
  show (Regex _ pat flags) =
    "Regex {pattern = " ++ show (BS8.unpack pat) ++
    ", flags = " ++ show flags ++ "}"

-- | A successful match result
data Match = Match
  { matchStart :: Int      -- ^ Starting position of the match
  , matchEnd :: Int        -- ^ Ending position of the match
  , matchText :: BS.ByteString  -- ^ The matched text
  , captures :: [(Int, Int, BS.ByteString)]  -- ^ Capture groups (start, end, text)
  } deriving (Eq, Show, Generic)

-- | Re-export regex flags
type RegexFlag = I.RegexFlag
pattern Global :: RegexFlag
pattern Global = I.Global
pattern IgnoreCase :: RegexFlag
pattern IgnoreCase = I.IgnoreCase
pattern Multiline :: RegexFlag
pattern Multiline = I.Multiline
pattern DotAll :: RegexFlag
pattern DotAll = I.DotAll
pattern Unicode :: RegexFlag
pattern Unicode = I.Unicode
pattern Sticky :: RegexFlag
pattern Sticky = I.Sticky
pattern Indices :: RegexFlag
pattern Indices = I.Indices
pattern UnicodeSets :: RegexFlag
pattern UnicodeSets = I.UnicodeSets

-- | Compile a regular expression pattern from a ByteString
--
-- Returns 'Left' with an error message if compilation fails,
-- or 'Right' with a compiled 'Regex' if successful.
compile :: BS.ByteString -> [RegexFlag] -> IO (Either String Regex)
compile pat flags = do
  result <- I.compileRegex pat flags
  case result of
    Left err -> return $ Left err
    Right ptr -> do
      fptr <- newForeignPtr I.c_free ptr
      return $ Right $ Regex fptr pat flags

-- | Compile a regular expression pattern from a Text value
compileText :: T.Text -> [RegexFlag] -> IO (Either String Regex)
compileText pat = compile (TE.encodeUtf8 pat)

-- | Test if a regex matches a subject string
--
-- Returns 'True' if the pattern matches anywhere in the subject.
test :: Regex -> BS.ByteString -> IO Bool
test regex subject = isJust <$> match regex subject

-- | Find the first match of a regex in a subject string
--
-- Returns 'Nothing' if no match is found, or 'Just Match' with the
-- match details and capture groups.
match :: Regex -> BS.ByteString -> IO (Maybe Match)
match = matchFrom 0

-- | Find the first match starting from a specific position
matchFrom :: Int -> Regex -> BS.ByteString -> IO (Maybe Match)
matchFrom startIdx (Regex fptr _ _) subject = do
  withForeignPtr fptr $ \ptr -> do
    result <- I.execRegex ptr subject startIdx
    case result of
      Nothing -> return Nothing
      Just (start, end, rawCaptures) -> do
        let matchedText = BS.take (end - start) $ BS.drop start subject
        let processedCaptures = map (processCapture subject) rawCaptures
        return $ Just $ Match start end matchedText processedCaptures

-- | Helper to extract capture group text
processCapture :: BS.ByteString -> (Int, Int) -> (Int, Int, BS.ByteString)
processCapture subject (start, end)
  | start < 0 || end < 0 = (start, end, BS.empty)  -- Unmatched group
  | otherwise = (start, end, BS.take (end - start) $ BS.drop start subject)

-- | Find all non-overlapping matches in a subject string
matchAll :: Regex -> BS.ByteString -> IO [Match]
matchAll regex subject = go 0
  where
    go pos
      | pos >= BS.length subject = return []
      | otherwise = do
          result <- matchFrom pos regex subject
          case result of
            Nothing -> return []
            Just m@(Match start end _ _) -> do
              -- Move past this match, or advance by 1 if empty match
              let nextPos = if end > start then end else start + 1
              rest <- go nextPos
              return (m : rest)

-- | Get the number of capture groups in a compiled regex
-- Returns the count of explicit capture groups (not including the full match)
getCaptureCount :: Regex -> IO Int
getCaptureCount (Regex fptr _ _) = do
  count <- withForeignPtr fptr I.getCaptureCount
  -- libregexp includes the full match in its count, subtract 1
  return (count - 1)

-- | Get the flags used to compile the regex
getFlags :: Regex -> IO [RegexFlag]
getFlags (Regex fptr _ _) =
  withForeignPtr fptr I.getFlags

-- | Get named capture group names (if any)
getGroupNames :: Regex -> IO (Maybe String)
getGroupNames (Regex fptr _ _) =
  withForeignPtr fptr I.getGroupNames
