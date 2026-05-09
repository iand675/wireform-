{-# LANGUAGE OverloadedStrings #-}

-- | Punycode encoding and decoding per RFC 3492.
--
-- Punycode is an encoding scheme used to represent Unicode strings in ASCII,
-- primarily for use in Internationalized Domain Names (IDN). This module provides
-- a pure Haskell implementation of the Punycode algorithm.
--
-- == Example
--
-- >>> encode "münchen"
-- Right "mnchen-3ya"
--
-- >>> decode "mnchen-3ya"
-- Right "münchen"
--
-- == See Also
--
-- * 'Data.Text.IDN' for high-level domain name processing
-- * RFC 3492 for the full specification
module Data.Text.Punycode
  ( -- * Encoding and Decoding
    encode
  , decode
    
    -- * Error Types
  , PunycodeError(..)
  ) where

import Data.Text (Text)
import Data.Text.IDN.Internal.Punycode (encodePunycode, decodePunycode)
import Data.Text.IDN.Types (PunycodeError(..))

-- | Encode a Unicode string to Punycode.
--
-- Converts a Unicode string into its Punycode representation. The result
-- contains only ASCII characters (a-z, 0-9, and the delimiter '-') and can
-- be safely used in contexts that require ASCII-only strings.
--
-- === Examples
--
-- >>> encode "münchen"
-- Right "mnchen-3ya"
--
-- >>> encode "北京"
-- Right "1ci"
--
-- === Errors
--
-- Returns 'Left' with 'PunycodeError' if:
-- * The input contains invalid Unicode code points
-- * Integer overflow occurs during encoding
--
-- @since 0.1.0.0
encode :: Text -> Either PunycodeError Text
encode = encodePunycode

-- | Decode a Punycode string back to Unicode.
--
-- Converts a Punycode-encoded string back to its original Unicode representation.
-- The input should contain only ASCII characters (a-z, 0-9, and '-').
--
-- === Examples
--
-- >>> decode "mnchen-3ya"
-- Right "münchen"
--
-- >>> decode "1ci"
-- Right "北京"
--
-- === Errors
--
-- Returns 'Left' with 'PunycodeError' if:
-- * The input is malformed Punycode
-- * Integer overflow occurs during decoding
--
-- @since 0.1.0.0
decode :: Text -> Either PunycodeError Text
decode = decodePunycode

