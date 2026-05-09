{-# LANGUAGE OverloadedStrings #-}

-- | Internal IDN contextual and bidirectional validation rules.
--
-- This module provides validation functions used internally for IDN processing.
-- For public API, see 'Data.Text.IDN'.
module Data.Text.IDN.Internal.Validation
  ( validateContextualRules
  , validateBidiRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IDN.Types
import Data.Text.IDN.Internal.Unicode
import Data.Char (ord)

-- | Validate contextual rules for CONTEXTJ and CONTEXTO code points (RFC 5892 Appendix A)
validateContextualRules :: Text -> Either IDNError ()
validateContextualRules label = go 0
  where
    len = T.length label
    go pos
      | pos >= len = pure ()
      | otherwise = do
          let c = T.index label pos
          case codePointStatus c of
            CONTEXTJ -> validateContextJ label pos c
            CONTEXTO -> validateContextO label pos c
            _ -> pure ()
          go (pos + 1)

-- | Validate CONTEXTJ characters (Join Controls)
validateContextJ :: Text -> Int -> Char -> Either IDNError ()
validateContextJ label pos c
  | ord c == 0x200C = validateZWNJ label pos c  -- ZERO WIDTH NON-JOINER
  | ord c == 0x200D = validateZWJ label pos c   -- ZERO WIDTH JOINER
  | otherwise = pure ()

-- | Validate CONTEXTO characters (Other)
validateContextO :: Text -> Int -> Char -> Either IDNError ()
validateContextO label pos c =
  case lookupContextRule c of
    Just MiddleDotRule -> validateMiddleDot label pos c
    Just GreekKeraiaRule -> validateGreekKeraia label pos c
    Just HebrewGereshRule -> validateHebrewPunctuation label pos c
    Just HebrewGershayimRule -> validateHebrewPunctuation label pos c
    Just KatakanaMiddleDotRule -> validateKatakanaMiddleDot label pos c
    Just ArabicIndicDigitsRule -> validateArabicIndicDigits label pos c
    Just ExtendedArabicIndicDigitsRule -> validateExtendedArabicIndicDigits label pos c
    _ -> pure ()

-- | ZERO WIDTH NON-JOINER (U+200C) - RFC 5892 Appendix A.1
-- Allowed after Virama OR in Arabic script contexts matching the regexp
validateZWNJ :: Text -> Int -> Char -> Either IDNError ()
validateZWNJ label pos c =
  let precededByVirama = pos > 0 && isVirama (T.index label (pos - 1))
      -- Arabic context: check if surrounded by Arabic joining characters
      -- Per W3C ALREQ, ZWNJ is used in Arabic between joining letters
      inArabicContext = pos > 0 && pos < T.length label - 1 &&
                       isArabicJoining (T.index label (pos - 1)) &&
                       isArabicJoining (T.index label (pos + 1))
  in if precededByVirama || inArabicContext
       then pure ()
       else Left (InvalidContext c ZWNJRule pos label)
  where
    -- Characters in Arabic script that can join (simplified check)
    isArabicJoining ch = scriptOf ch == Just Arabic && ord ch >= 0x0620 && ord ch <= 0x06FF

-- | ZERO WIDTH JOINER (U+200D) - RFC 5892 Appendix A.2
validateZWJ :: Text -> Int -> Char -> Either IDNError ()
validateZWJ label pos c =
  if pos > 0 && isVirama (T.index label (pos - 1))
    then pure ()
    else Left (InvalidContext c ZWJRule pos label)

-- | MIDDLE DOT (U+00B7) - RFC 5892 Appendix A.3
validateMiddleDot :: Text -> Int -> Char -> Either IDNError ()
validateMiddleDot label pos c =
  let before = pos > 0 && T.index label (pos - 1) == 'l'
      after = pos < T.length label - 1 && T.index label (pos + 1) == 'l'
  in if before && after
       then pure ()
       else Left (InvalidContext c MiddleDotRule pos label)

-- | GREEK LOWER NUMERAL SIGN (U+0375) - RFC 5892 Appendix A.4
-- The script of the following character MUST be Greek
validateGreekKeraia :: Text -> Int -> Char -> Either IDNError ()
validateGreekKeraia label pos c =
  let followedByGreek = pos < T.length label - 1 &&
                       scriptOf (T.index label (pos + 1)) == Just Greek
  in if followedByGreek
       then pure ()
       else Left (InvalidContext c GreekKeraiaRule pos label)

-- | HEBREW PUNCTUATION (U+05F3, U+05F4) - RFC 5892 Appendix A.5/A.6
-- The script of the preceding character MUST be Hebrew
validateHebrewPunctuation :: Text -> Int -> Char -> Either IDNError ()
validateHebrewPunctuation label pos c =
  let precededByHebrew = pos > 0 &&
                        scriptOf (T.index label (pos - 1)) == Just Hebrew
  in if precededByHebrew
       then pure ()
       else Left (InvalidContext c HebrewGereshRule pos label)

-- | KATAKANA MIDDLE DOT (U+30FB) - RFC 5892 Appendix A.7
validateKatakanaMiddleDot :: Text -> Int -> Char -> Either IDNError ()
validateKatakanaMiddleDot label pos c =
  let hasKatakanaOrHiragana = T.any (\ch -> 
        let s = scriptOf ch
        in s == Just Katakana || s == Just Hiragana || s == Just Han
        ) label
  in if hasKatakanaOrHiragana
       then pure ()
       else Left (InvalidContext c KatakanaMiddleDotRule pos label)

-- | ARABIC-INDIC DIGITS (U+0660..U+0669) - RFC 5892 Appendix A.8
validateArabicIndicDigits :: Text -> Int -> Char -> Either IDNError ()
validateArabicIndicDigits label pos c =
  let hasExtended = T.any (\ch -> ord ch >= 0x06F0 && ord ch <= 0x06F9) label
  in if not hasExtended
       then pure ()
       else Left (InvalidContext c ArabicIndicDigitsRule pos label)

-- | EXTENDED ARABIC-INDIC DIGITS (U+06F0..U+06F9) - RFC 5892 Appendix A.9
validateExtendedArabicIndicDigits :: Text -> Int -> Char -> Either IDNError ()
validateExtendedArabicIndicDigits label pos c =
  let hasNormal = T.any (\ch -> ord ch >= 0x0660 && ord ch <= 0x0669) label
  in if not hasNormal
       then pure ()
       else Left (InvalidContext c ExtendedArabicIndicDigitsRule pos label)

-- | Validate bidirectional text rules (RFC 5893)
validateBidiRules :: Text -> Either IDNError ()
validateBidiRules label
  | T.null label = pure ()
  | otherwise =
      let hasRTL = T.any (\c -> let bc = bidiClass c in bc == R || bc == AL || bc == AN) label
      in if hasRTL
           then validateRTLLabel label
           else pure ()  -- LTR labels have minimal bidi restrictions

-- | Validate RTL labels (RFC 5893 Rules 2-6)
validateRTLLabel :: Text -> Either IDNError ()
validateRTLLabel label = do
  let len = T.length label
  
  -- Rule 2: First character must be R or AL
  let firstBC = bidiClass (T.index label 0)
  if firstBC `notElem` [R, AL]
    then Left (BidiViolation Rule2 label)
    else pure ()
  
  -- Rule 3: Last character must be R, AL, EN, or AN
  let lastBC = bidiClass (T.index label (len - 1))
  if lastBC `notElem` [R, AL, EN, AN]
    then Left (BidiViolation Rule3 label)
    else pure ()
  
  -- Rule 4: NSM must follow allowed characters
  checkNSMPositions 0 len
  
  -- Rules 5 & 6: Collect bidi class flags in a single pass
  let flags = T.foldl' checkBidiClass (BidiFlags False False False) label
  
  -- Rule 5: No ES or CS with AN
  if bfHasAN flags && bfHasESorCS flags
    then Left (BidiViolation Rule5 label)
    else pure ()
  
  -- Rule 6: EN and AN cannot be mixed
  if bfHasEN flags && bfHasAN flags
    then Left (BidiViolation Rule6 label)
    else pure ()
  
  where
    checkNSMPositions pos len
      | pos >= len = pure ()
      | otherwise = do
          let currentBC = bidiClass (T.index label pos)
          if currentBC == NSM && pos > 0
            then do
              let prevBC = bidiClass (T.index label (pos - 1))
              if prevBC `notElem` [R, AL, EN, AN]
                then Left (BidiViolation Rule4 label)
                else checkNSMPositions (pos + 1) len
            else checkNSMPositions (pos + 1) len
    
    checkBidiClass acc c =
      let bc = bidiClass c
      in BidiFlags
           { bfHasAN = bfHasAN acc || bc == AN
           , bfHasEN = bfHasEN acc || bc == EN
           , bfHasESorCS = bfHasESorCS acc || bc == ES || bc == CS
           }

-- | Strict accumulator for collecting bidi class information in a single pass
data BidiFlags = BidiFlags
  { bfHasAN     :: {-# UNPACK #-} !Bool  -- ^ Has Arabic-Indic digits (AN)
  , bfHasEN     :: {-# UNPACK #-} !Bool  -- ^ Has European numbers (EN)
  , bfHasESorCS :: {-# UNPACK #-} !Bool  -- ^ Has European or Common separators (ES/CS)
  }

