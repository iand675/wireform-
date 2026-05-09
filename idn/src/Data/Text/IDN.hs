{-# LANGUAGE OverloadedStrings #-}

-- | High-level API for Internationalized Domain Names (IDN) processing.
--
-- This module provides functions for converting between Unicode and ASCII
-- domain names, following the IDNA2008 specification (RFC 5890-5893).
--
-- == Overview
--
-- Internationalized Domain Names allow domain names to contain non-ASCII
-- characters. This library implements IDNA2008, which uses Punycode encoding
-- to represent Unicode characters in ASCII-compatible form.
--
-- == Example
--
-- >>> toASCII "münchen.de"
-- Right "xn--mnchen-3ya.de"
--
-- >>> toUnicode "xn--mnchen-3ya.de"
-- Right "münchen.de"
--
-- >>> validateLabel "example"
-- Right ()
--
-- == See Also
--
-- * 'Data.Text.Punycode' for low-level Punycode encoding/decoding
-- * RFC 5890-5893 for the full IDNA2008 specification
module Data.Text.IDN
  ( -- * Main API
    toASCII
  , toUnicode
  , validateLabel
  
    -- * Types
  , module Data.Text.IDN.Types
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IDN.Types
import Data.Text.IDN.Internal.Unicode
import Data.Text.IDN.Internal.Validation
import Data.Text.IDN.Internal.Punycode (encodePunycode, decodePunycode)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Char (ord, isAscii)

-- | Convert a Unicode domain name to ASCII (A-label) form per RFC 5891.
--
-- Converts each label in the domain name to its ASCII-compatible form.
-- Unicode labels are encoded using Punycode and prefixed with "xn--".
-- ASCII labels are normalized to lowercase (case-insensitive per DNS).
--
-- === Examples
--
-- >>> toASCII "münchen.de"
-- Right "xn--mnchen-3ya.de"
--
-- >>> toASCII "EXAMPLE.COM"
-- Right "example.com"
--
-- === Errors
--
-- Returns 'Left' with 'IDNError' if:
-- * Any label is empty or too long
-- * Any label contains disallowed code points
-- * Bidirectional text rules are violated
-- * Contextual validation fails
--
-- @since 0.1.0.0
toASCII :: Text -> Either IDNError Text
toASCII input = do
  domain <- mkDomainName input
  processedLabels <- traverse processLabelToASCII (labels domain)
  return $ T.intercalate "." $ map labelText (NE.toList processedLabels)
  where
    labelText (ULabel t) = t
    labelText (ALabel t) = t

-- | Convert an ASCII domain name to Unicode (U-label) form per RFC 5891.
--
-- Converts each label in the domain name to its Unicode form.
-- Punycode-encoded labels (starting with "xn--") are decoded to Unicode.
-- Pure ASCII labels are validated and passed through unchanged.
--
-- === Examples
--
-- >>> toUnicode "xn--mnchen-3ya.de"
-- Right "münchen.de"
--
-- >>> toUnicode "example.com"
-- Right "example.com"
--
-- === Errors
--
-- Returns 'Left' with 'IDNError' if:
-- * Any label is empty or too long
-- * Punycode decoding fails
-- * Any label contains disallowed code points
-- * Bidirectional text rules are violated
--
-- @since 0.1.0.0
toUnicode :: Text -> Either IDNError Text
toUnicode input = do
  domain <- mkDomainName input
  processedLabels <- traverse processLabelToUnicode (labels domain)
  return $ T.intercalate "." $ map labelText (NE.toList processedLabels)
  where
    labelText (ULabel t) = t
    labelText (ALabel t) = t

-- | Process a single label to ASCII form.
processLabelToASCII :: Label -> Either IDNError Label
processLabelToASCII (ALabel t)
  -- Labels are already normalized to lowercase by mkLabel
  | "xn--" `T.isPrefixOf` t = do
      -- It's a Punycode A-label, validate it by decoding and re-encoding
      -- This uses processLabelToUnicode which validates Punycode properly
      ulabel <- processLabelToUnicode (ALabel t)
      -- Then re-process back to ASCII to get a validated A-label
      processLabelToASCII ulabel
  | otherwise =
      -- Regular ASCII label (already normalized), just validate
      validateLabel t >> return (ALabel t)

processLabelToASCII (ULabel t)
  | isAsciiLabel t = do
      -- Pure ASCII label (already normalized), just validate
      validateLabel t
      return (ULabel t)
  
  | otherwise = do
      -- Unicode label, needs Punycode encoding
      validateLabel t
      encoded <- case encodePunycode t of
        Left err -> Left (PunycodeDecodeError t (T.pack (show err)))
        Right e -> Right e
      let aLabel = "xn--" <> encoded
      
      -- Validate length constraints
      if T.length aLabel > 63
        then Left (LabelTooLong (T.length aLabel) 63)
        else return (ALabel aLabel)

-- | Process a single label to Unicode form.
processLabelToUnicode :: Label -> Either IDNError Label
processLabelToUnicode (ULabel t) =
  -- Already Unicode, validate and return
  validateLabel t >> return (ULabel t)

processLabelToUnicode (ALabel t)
  -- RFC 5891 Section 5.1: A-labels are case-insensitive for matching
  | "xn--" `T.isPrefixOf` T.toLower t = do
      -- Punycode A-label, decode it
      -- RFC 5891 Section 5.1: Accept case-insensitive input, normalize to lowercase
      -- Drop the prefix (which might be XN--, Xn--, xN--, or xn--)
      let encoded = T.drop 4 t
          encodedLower = T.toLower encoded

      decoded <- case decodePunycode encodedLower of
        Left err -> Left (PunycodeDecodeError encoded (T.pack (show err)))
        Right d -> Right d

      -- Punycode-encoded labels must contain at least one non-ASCII character
      -- If the decoded result is pure ASCII, it shouldn't have been encoded
      if isAsciiLabel decoded
        then Left (PunycodeDecodeError encoded "Punycode used for pure ASCII label")
        else pure ()

      -- RFC 5891 Section 4.4: Verify round-trip consistency
      -- Re-encode and check it matches the lowercase normalized form
      reencoded <- case encodePunycode decoded of
        Left err -> Left (PunycodeDecodeError decoded (T.pack (show err)))
        Right e -> Right e
      if reencoded /= encodedLower
        then Left (PunycodeDecodeError encoded "Round-trip re-encoding failed")
        else do
          validateLabel decoded
          return (ULabel decoded)

  | otherwise =
      -- Pure ASCII label, validate and return
      validateLabel t >> return (ALabel t)

-- | Validate a single domain label per IDNA2008 rules.
--
-- Performs comprehensive validation including:
-- * Length constraints (max 63 characters)
-- * Hyphen position rules
-- * Code point validity (PVALID, CONTEXTJ, CONTEXTO)
-- * Contextual rule validation
-- * Bidirectional text rules (RFC 5893)
--
-- === Examples
--
-- >>> validateLabel "example"
-- Right ()
--
-- >>> validateLabel ""
-- Left EmptyLabel
--
-- === Errors
--
-- Returns 'Left' with 'IDNError' if validation fails. See 'IDNError'
-- constructors for specific error types.
--
-- @since 0.1.0.0
validateLabel :: Text -> Either IDNError ()
validateLabel label = do
  -- Check empty
  if T.null label
    then Left EmptyLabel
    else pure ()
  
  -- Check length (before encoding)
  if T.length label > 63
    then Left (LabelTooLong (T.length label) 63)
    else pure ()
  
  -- Check hyphen positions
  checkHyphens label
  
  -- Check combining marks at start
  case T.uncons label of
    Just (c, _) | isCombiningMark c ->
      Left (StartsWithCombiningMark c (ord c))
    _ -> pure ()
  
  -- Validate each code point
  mapM_ validateCodePoint (T.unpack label)
  
  -- Validate contextual rules for CONTEXTJ/CONTEXTO
  validateContextualRules label
  
  -- Validate bidi rules
  validateBidiRules label
  
  return ()

-- | Check hyphen position rules.
checkHyphens :: Text -> Either IDNError ()
checkHyphens label = do
  case T.uncons label of
    Just ('-', _) -> Left (InvalidHyphenPosition StartsWithHyphen)
    _ -> pure ()

  case T.unsnoc label of
    Just (_, '-') -> Left (InvalidHyphenPosition EndsWithHyphen)
    _ -> pure ()

  -- Check for "--" in positions 3-4 (unless it's an A-label with xn--)
  -- RFC 5891: A-labels are case-insensitive for matching
  let labelLower = T.toLower label
  if T.length label >= 4 && not ("xn--" `T.isPrefixOf` labelLower)
    then if T.index label 2 == '-' && T.index label 3 == '-'
      then Left (InvalidHyphenPosition HyphensAt3And4NotPunycode)
      else pure ()
    else pure ()

-- | Validate a single code point.
validateCodePoint :: Char -> Either IDNError ()
validateCodePoint c =
  case codePointStatus c of
    PVALID -> pure ()
    DISALLOWED -> Left (DisallowedCodePoint c (ord c))
    CONTEXTJ -> pure ()  -- Will be checked in context validation
    CONTEXTO -> pure ()  -- Will be checked in context validation
    UNASSIGNED -> Left (DisallowedCodePoint c (ord c))

-- | Check if a label is pure ASCII.
isAsciiLabel :: Text -> Bool
isAsciiLabel = T.all isAscii

