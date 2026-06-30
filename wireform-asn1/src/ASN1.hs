{- | Convenience re-export module for the common wireform-asn1 surface.

Import this module to get the ASN.1 BER/DER codec — encoding, decoding, the
dynamic value model, and the deriver — in one go.

@
import ASN1
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "ASN1.Schema" (IDL schema model),
"ASN1.Parser" (the low-level BER/DER parser), "ASN1.CodeGen" (template
Haskell code generation), and "ASN1.QQ" (quasiquoter).
-}
module ASN1 (
  -- * Encoding
  module ASN1.Encode,

  -- * Decoding
  module ASN1.Decode,

  -- * Dynamic values
  module ASN1.Value,

  -- * Deriving
  --
  -- Re-exported explicitly (rather than @module ASN1.Derive@) to drop the
  -- 'ASN1.Derive.Asn1Tag' constructor @Universal@, which collides with
  -- 'ASN1.Value.TagClass'\'s @Universal@; the value-model constructor wins
  -- per the entry-module precedence rule (see @docs/PLATFORM.md@ §4). Build
  -- the deriver tag via 'asn1Universal' instead of the raw constructor.
  ToASN1 (..),
  FromASN1 (..),
  encodeASN1,
  decodeASN1,
  deriveASN1,
  deriveToASN1,
  deriveFromASN1,
  Asn1Tag (Implicit, Explicit),
  asn1ImplicitTag,
  asn1ExplicitTag,
  asn1Universal,
) where

import ASN1.Decode
import ASN1.Derive
import ASN1.Encode
import ASN1.Value
