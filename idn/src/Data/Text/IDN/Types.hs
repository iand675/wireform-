{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core types for Internationalized Domain Names (IDN) processing.
--
-- This module provides types for domain name processing following the IDNA2008
-- specification (RFC 5890-5893). Types are organized into logical groups:
-- errors, domain structures, Unicode properties, and Punycode internals.
--
-- == Example
--
-- >>> mkLabel "example"
-- Right (ULabel "example")
--
-- >>> mkDomainName "example.com"
-- Right (DomainName {labels = ...})
module Data.Text.IDN.Types
  ( -- * Error Types
    IDNError(..)
  , HyphenError(..)
  , PunycodeError(..)
  
    -- * Domain Types
  , Label(..)
  , DomainName(..)
  , mkLabel
  , mkDomainName
  
    -- * Unicode Property Types
  , CodePointStatus(..)
  , BidiClass(..)
  , ContextRule(..)
  , BidiRule(..)
  , Script(..)
  
    -- * Punycode Types
  , PunycodeState(..)
  , PunycodeParams(..)
  , initialState
  , punycodeParams
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | IDN processing errors with detailed context.
--
-- Errors include validation failures, encoding issues, and constraint violations
-- per IDNA2008 (RFC 5890-5893).
data IDNError
  = EmptyLabel
    -- ^ Domain label cannot be empty
  | LabelTooLong 
      { actualLength :: Int
        -- ^ Actual label length
      , maxLength :: Int
        -- ^ Maximum allowed length (63)
      }
    -- ^ Domain label exceeds maximum length
  | TotalDomainTooLong
      { actualLength :: Int
        -- ^ Actual domain length
      , maxLength :: Int
        -- ^ Maximum allowed length (253)
      }
    -- ^ Complete domain name exceeds maximum length
  | InvalidHyphenPosition HyphenError
    -- ^ Hyphen appears in invalid position
  | StartsWithCombiningMark
      { character :: Char
        -- ^ The combining mark character
      , codepoint :: Int
        -- ^ Unicode code point value
      }
    -- ^ Label starts with a combining mark (invalid)
  | DisallowedCodePoint
      { character :: Char
        -- ^ The disallowed character
      , codepoint :: Int
        -- ^ Unicode code point value
      }
    -- ^ Code point is not allowed in IDN labels
  | InvalidContext
      { character :: Char
        -- ^ The character that failed context validation
      , contextRule :: ContextRule
        -- ^ The specific context rule violated
      , position :: Int
        -- ^ Position in label where error occurred
      , context :: Text
        -- ^ The full label context
      }
    -- ^ Contextual rule violation (CONTEXTJ/CONTEXTO)
  | BidiViolation
      { bidiRule :: BidiRule
        -- ^ The bidirectional rule violated
      , label :: Text
        -- ^ The label that failed validation
      }
    -- ^ Bidirectional text rule violation (RFC 5893)
  | PunycodeDecodeError
      { input :: Text
        -- ^ The input that failed to decode
      , reason :: Text
        -- ^ Reason for decode failure
      }
    -- ^ Error during Punycode decoding
  deriving (Show, Eq, Generic)

instance NFData IDNError

-- | Hyphen position errors.
--
-- Domain labels cannot start or end with hyphens, and hyphens at positions
-- 3-4 are reserved for Punycode encoding.
data HyphenError
  = StartsWithHyphen
    -- ^ Label starts with hyphen (invalid)
  | EndsWithHyphen
    -- ^ Label ends with hyphen (invalid)
  | HyphensAt3And4NotPunycode
    -- ^ Hyphens at positions 3-4 but not a valid Punycode label
  deriving (Show, Eq, Generic)

instance NFData HyphenError

-- | Punycode encoding/decoding errors.
--
-- These errors occur during low-level Punycode operations. For domain-level
-- errors, see 'IDNError'.
data PunycodeError
  = InvalidPunycode Text
    -- ^ Malformed Punycode string with reason
  | Overflow
    -- ^ Integer overflow during encoding/decoding
  deriving (Show, Eq, Generic)

instance NFData PunycodeError

-- | Domain label in either Unicode or ASCII/Punycode form.
--
-- Labels are the components of a domain name separated by dots. They can be
-- in Unicode form (U-label) or ASCII/Punycode form (A-label).
data Label
  = ULabel Text
    -- ^ Unicode label (U-label) - contains Unicode characters
  | ALabel Text
    -- ^ ASCII/Punycode label (A-label) - contains ASCII or Punycode with xn-- prefix
  deriving (Show, Eq, Generic)

instance NFData Label

-- | Complete domain name consisting of one or more labels.
--
-- A domain name is a non-empty sequence of labels separated by dots.
-- The total length must not exceed 253 characters.
newtype DomainName = DomainName 
  { labels :: NonEmpty Label
    -- ^ Non-empty list of domain labels
  }
  deriving (Show, Eq, Generic)

instance NFData DomainName

-- | Smart constructor for 'Label' with invariant enforcement.
--
-- Validates that the label is non-empty and within length constraints.
-- Automatically detects A-labels (those starting with "xn--").
--
-- === Examples
--
-- >>> mkLabel "example"
-- Right (ULabel "example")
--
-- >>> mkLabel "xn--mnchen-3ya"
-- Right (ALabel "xn--mnchen-3ya")
--
-- >>> mkLabel ""
-- Left EmptyLabel
mkLabel :: Text -> Either IDNError Label
mkLabel text
  | T.null text = Left EmptyLabel
  | T.length text > 63 = Left (LabelTooLong (T.length text) 63)
  | otherwise =
      -- RFC 5891/3490: Domain labels are case-insensitive, normalize to lowercase
      let normalized = T.toLower text
      -- RFC 5891 Section 5.1: A-labels start with "xn--"
      in if "xn--" `T.isPrefixOf` normalized
           then Right (ALabel normalized)
           else Right (ULabel normalized)

-- | Smart constructor for 'DomainName' with invariant enforcement.
--
-- Validates that the domain is non-empty, within length constraints, and
-- properly formatted. Splits on dots to create individual labels.
--
-- === Examples
--
-- >>> mkDomainName "example.com"
-- Right (DomainName {labels = ...})
--
-- >>> mkDomainName ""
-- Left EmptyLabel
mkDomainName :: Text -> Either IDNError DomainName
mkDomainName text = do
  if T.null text
    then Left EmptyLabel
    else if T.length text > 253
      then Left (TotalDomainTooLong (T.length text) 253)
      else do
        -- RFC 3490 Section 3.1: Normalize alternative dot separators
        -- U+3002 IDEOGRAPHIC FULL STOP, U+FF0E FULLWIDTH FULL STOP, U+FF61 HALFWIDTH IDEOGRAPHIC FULL STOP
        let normalized = T.map normalizeDot text
            labelTexts = T.splitOn "." normalized
        labelList <- traverse mkLabel labelTexts
        case NE.nonEmpty labelList of
          Nothing -> Left EmptyLabel
          Just ne -> Right (DomainName ne)
  where
    normalizeDot '\x3002' = '.'  -- IDEOGRAPHIC FULL STOP
    normalizeDot '\xFF0E' = '.'  -- FULLWIDTH FULL STOP
    normalizeDot '\xFF61' = '.'  -- HALFWIDTH IDEOGRAPHIC FULL STOP
    normalizeDot c = c

-- | IDNA2008 code point status per RFC 5892.
--
-- Each Unicode code point is classified according to its validity for use
-- in IDN labels.
data CodePointStatus
  = PVALID
    -- ^ Always valid in domain names
  | DISALLOWED
    -- ^ Never valid in domain names
  | CONTEXTJ
    -- ^ Valid only in specific contexts (Join_Control)
  | CONTEXTO
    -- ^ Valid only in specific contexts (Other)
  | UNASSIGNED
    -- ^ Not assigned in this Unicode version
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData CodePointStatus

-- | Unicode bidirectional character class per Unicode Character Database (UCD).
--
-- Used for bidirectional text validation per RFC 5893.
data BidiClass
  = L    -- ^ Left-to-Right
  | R    -- ^ Right-to-Left
  | AL   -- ^ Right-to-Left Arabic
  | AN   -- ^ Arabic Number
  | EN   -- ^ European Number
  | ES   -- ^ European Number Separator
  | ET   -- ^ European Number Terminator
  | CS   -- ^ Common Number Separator
  | NSM  -- ^ Nonspacing Mark
  | BN   -- ^ Boundary Neutral
  | B    -- ^ Paragraph Separator
  | S    -- ^ Segment Separator
  | WS   -- ^ Whitespace
  | ON   -- ^ Other Neutral
  | LRE  -- ^ Left-to-Right Embedding
  | LRO  -- ^ Left-to-Right Override
  | RLE  -- ^ Right-to-Left Embedding
  | RLO  -- ^ Right-to-Left Override
  | PDF  -- ^ Pop Directional Format
  | LRI  -- ^ Left-to-Right Isolate
  | RLI  -- ^ Right-to-Left Isolate
  | FSI  -- ^ First Strong Isolate
  | PDI  -- ^ Pop Directional Isolate
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData BidiClass

-- | Contextual validation rules from RFC 5892 Appendix A.
--
-- Some code points are only valid in specific contexts. These rules define
-- those contexts.
data ContextRule
  = ZWNJRule
    -- ^ A.1: Zero Width Non-Joiner (U+200C)
  | ZWJRule
    -- ^ A.2: Zero Width Joiner (U+200D)
  | MiddleDotRule
    -- ^ A.3: Middle Dot (U+00B7)
  | GreekKeraiaRule
    -- ^ A.4: Greek Lower Numeral Sign (U+0375)
  | HebrewGereshRule
    -- ^ A.5: Hebrew Punctuation Geresh (U+05F3)
  | HebrewGershayimRule
    -- ^ A.6: Hebrew Punctuation Gershayim (U+05F4)
  | KatakanaMiddleDotRule
    -- ^ A.7: Katakana Middle Dot (U+30FB)
  | ArabicIndicDigitsRule
    -- ^ A.8: Arabic-Indic Digits (U+0660..U+0669)
  | ExtendedArabicIndicDigitsRule
    -- ^ A.9: Extended Arabic-Indic Digits (U+06F0..U+06F9)
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData ContextRule

-- | Bidirectional text rules from RFC 5893.
--
-- These rules ensure proper bidirectional text handling in RTL labels.
data BidiRule
  = Rule1
    -- ^ Presence of RTL characters triggers RTL validation
  | Rule2
    -- ^ First character must be R or AL
  | Rule3
    -- ^ Last character must be R, AL, AN, or EN
  | Rule4
    -- ^ NSM may only follow allowed character types
  | Rule5
    -- ^ ES and CS constraints in RTL labels
  | Rule6
    -- ^ EN and AN cannot be mixed
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData BidiRule

-- | Unicode script assignment.
--
-- Scripts are used for contextual validation of certain code points.
data Script
  = Latin
  | Greek
  | Cyrillic
  | Hebrew
  | Arabic
  | Devanagari
  | Bengali
  | Gurmukhi
  | Gujarati
  | Oriya
  | Tamil
  | Telugu
  | Kannada
  | Malayalam
  | Thai
  | Lao
  | Tibetan
  | Myanmar
  | Georgian
  | Hangul
  | Hiragana
  | Katakana
  | Han
  | OtherScript
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData Script

-- | Punycode algorithm state per RFC 3492.
--
-- Internal state used during Punycode encoding/decoding. Not typically
-- needed by users of the library.
data PunycodeState = PunycodeState
  { n :: {-# UNPACK #-}!Int
    -- ^ Next code point to encode
  , delta :: {-# UNPACK #-}!Int
    -- ^ Delta value for encoding
  , bias :: {-# UNPACK #-}!Int
    -- ^ Bias for variable-length integer encoding
  , h :: {-# UNPACK #-}!Int
    -- ^ Number of code points handled
  }
  deriving (Show, Eq, Generic)

instance NFData PunycodeState

-- | Initial Punycode state per RFC 3492.
--
-- Starting state for Punycode encoding operations.
initialState :: PunycodeState
initialState = PunycodeState
  { n = 0x80      -- Initial code point (128)
  , delta = 0
  , bias = 72     -- Initial bias
  , h = 0
  }

-- | Punycode algorithm parameters per RFC 3492.
--
-- Constants used in the Punycode algorithm. Not typically needed by users.
data PunycodeParams = PunycodeParams
  { base :: Int   -- ^ Base (36)
  , tmin :: Int   -- ^ Minimum threshold (1)
  , tmax :: Int   -- ^ Maximum threshold (26)
  , skew :: Int   -- ^ Skew (38)
  , damp :: Int   -- ^ Damp (700)
  }
  deriving (Show, Eq, Generic)

instance NFData PunycodeParams

-- | Standard Punycode parameters from RFC 3492.
punycodeParams :: PunycodeParams
punycodeParams = PunycodeParams
  { base = 36
  , tmin = 1
  , tmax = 26
  , skew = 38
  , damp = 700
  }

