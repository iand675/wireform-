{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoPatternSynonyms #-}
-- | Pure interface to ECMA262 regular expressions
--
-- This module provides an observably pure interface to the underlying
-- ECMA262 regex engine with a simple PCRE-like API.
--
-- The IO operations are encapsulated using unsafePerformIO which is safe because:
--
-- 1. Regex compilation is deterministic - same pattern and flags always produce the same result
-- 2. Regex matching is deterministic - same regex and input always produce the same result
-- 3. No observable side effects occur (no mutation, no external state changes)
-- 4. The FFI operations are thread-safe
module JsonSchema.Regex
  ( -- * Types
    Regex
  , RegexFlag(..)
    -- * Compilation
  , compile
  , compileText
  , makeRegex
  , compileRegex
    -- * Matching
  , test
  , matchFirst
  , matchRegex
  , (=~)
  ) where

import qualified Text.Regex.ECMA262 as R
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Unsafe (unsafePerformIO)
import qualified Numeric

-- | Re-export Regex type
type Regex = R.Regex

-- | Re-export RegexFlag type
type RegexFlag = R.RegexFlag

-- | Compile a regular expression pattern from a ByteString
--
-- This is observably pure: same input always produces same output,
-- no side effects, deterministic behavior.
compile :: BS.ByteString -> [RegexFlag] -> Either String Regex
compile pat flags = unsafePerformIO $ R.compile pat flags
{-# NOINLINE compile #-}

-- | Compile a regular expression pattern from a Text value
compileText :: T.Text -> [RegexFlag] -> Either String Regex
compileText pat flags = unsafePerformIO $ R.compileText pat flags
{-# NOINLINE compileText #-}

-- | Test if a regex matches a subject string
--
-- This is observably pure: same regex and input always produces same result.
test :: Regex -> BS.ByteString -> Bool
test regex subject = unsafePerformIO $ R.test regex subject
{-# NOINLINE test #-}

-- | Find the first match of a regex in a subject string
--
-- This is observably pure: same regex and input always produces same result.
matchFirst :: Regex -> BS.ByteString -> Maybe R.Match
matchFirst regex subject = unsafePerformIO $ R.match regex subject
{-# NOINLINE matchFirst #-}

-- | Make a regex from a string (throws error on invalid pattern)
makeRegex :: String -> Regex
makeRegex pat = case compile (BS8.pack pat) [] of
  Right r -> r
  Left err -> error $ "makeRegex: " ++ err

-- | PCRE-like matching operator
--
-- Usage:
-- @
--   "hello" =~ "^h.*o$" :: Bool
-- @
class MatchLike source pat where
  (=~) :: source -> pat -> Bool

-- Match String against String pattern
instance MatchLike String String where
  source =~ pat = case compile (BS8.pack pat) [] of
    Right regex -> test regex (BS8.pack source)
    Left _ -> False

-- Match Text against Text pattern
instance MatchLike T.Text T.Text where
  source =~ pat = case compileText pat [] of
    Right regex -> test regex (TE.encodeUtf8 source)
    Left _ -> False

-- Match String against compiled Regex
instance MatchLike String Regex where
  source =~ regex = test regex (BS8.pack source)

-- Match Text against compiled Regex
instance MatchLike T.Text Regex where
  source =~ regex = test regex (TE.encodeUtf8 source)

infixl 9 =~

-- | Compile regex pattern with JSON Schema semantics
-- Automatically adds Unicode flag if pattern contains Unicode property escapes
-- Escapes literal non-BMP characters to \u{...} format for proper matching
compileRegex :: T.Text -> Either T.Text Regex
compileRegex pattern =
  let escapedPattern = escapeNonBMPChars pattern
      -- Check original pattern to determine if Unicode mode is needed
      -- (escaped pattern is all ASCII, but we need Unicode mode for \u{...} escapes)
      flags = if needsUnicodeMode pattern then [R.Unicode] else []
  in case compileText escapedPattern flags of
      Left err -> Left $ T.pack $ "Invalid regex: " <> err
      Right regex -> Right regex

-- | Match text against regex (convenience wrapper)
matchRegex :: Regex -> T.Text -> Bool
matchRegex regex txt = test regex (TE.encodeUtf8 txt)

-- | Escape non-BMP characters (> U+FFFF) to \u{...} format
-- This is necessary because libregexp doesn't correctly match literal non-BMP
-- characters in UTF-8 patterns against UTF-16 data (surrogate pairs)
escapeNonBMPChars :: T.Text -> T.Text
escapeNonBMPChars = T.concatMap $ \c ->
  if fromEnum c > 0xFFFF
    then T.pack $ "\\u{" ++ Numeric.showHex (fromEnum c) "}"
    else T.singleton c

-- | Check if a pattern needs Unicode mode
-- Unicode property escapes (\p{...} or \P{...}) require Unicode mode
-- Character class escapes (\d, \D, \w, \W, \s, \S) need Unicode mode for proper multi-byte character matching
-- Literal non-ASCII characters in patterns also require Unicode mode
-- Also, patterns with non-BMP characters (code points > 0xFFFF) require Unicode mode
--
-- Why character classes and non-ASCII literals need Unicode mode:
-- In byte mode, libregexp treats each UTF-8 byte separately. For example, "é" (U+00E9)
-- is encoded as UTF-8 bytes [0xC3, 0xA9]. The pattern `^\\W$` or `^á$` expects exactly ONE
-- character, but in byte mode it sees TWO bytes. In Unicode mode (UTF-16), "é" is a single
-- code unit, so the pattern matches correctly.
needsUnicodeMode :: T.Text -> Bool
needsUnicodeMode pattern =
  -- Check for \p{...} or \P{...} patterns (after JSON parsing, these are single backslashes)
  -- We need to match against the actual text, which has the backslash
  let hasUnicodeProperty = any (`T.isInfixOf` pattern) ["\\p{", "\\P{"]
      -- Check if pattern contains any non-ASCII characters (>= U+0080)
      -- These need Unicode mode for proper matching as single code units
      hasNonASCII = T.any (\c -> fromEnum c >= 0x80) pattern
      -- Check for character class escapes that need Unicode mode
      hasCharClass = any (`T.isInfixOf` pattern) ["\\d", "\\D", "\\w", "\\W", "\\s", "\\S"]
  in hasUnicodeProperty || hasNonASCII || hasCharClass
