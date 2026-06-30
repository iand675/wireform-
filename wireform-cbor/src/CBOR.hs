{- | Convenience re-export module for the common wireform-cbor surface.

Import this module to get the CBOR codec — encoding, decoding, the
dynamic 'Value' model, the 'ToCBOR'/'FromCBOR' typeclasses, the
deriver, the direct-to-bytes 'Encoding' builder, streaming decode,
and RFC 8949 diagnostic notation — in one go.

@
import CBOR

let bs  = encode (TextString "hello")
case decode bs of
  Right val -> putStrLn (T.unpack (toDiagnostic val))
  Left  err -> putStrLn err
@

Specialized surfaces that are deliberately *not* re-exported here —
opt into them directly when you need them: "CBOR.JSON" (the
self-describing CBOR ↔ JSON bridge), "CBOR.QQ" (quasiquoter),
"CBOR.TagRegistry", and the CDDL schema modules ("CBOR.CDDL",
"CBOR.CDDLSchema", "CBOR.CDDLCodeGen").
-}
module CBOR (
  -- * Encoding
  module CBOR.Encode,
  module CBOR.Encoding,

  -- * Decoding
  module CBOR.Decode,
  module CBOR.Stream,

  -- * Dynamic values
  module CBOR.Value,

  -- * Typeclass-based codec + deriving
  module CBOR.Class,
  module CBOR.Derive,

  -- * Diagnostic notation (RFC 8949 §3.3 / §8)
  module CBOR.Diagnostic,
) where

import CBOR.Class
import CBOR.Decode
import CBOR.Derive
import CBOR.Diagnostic
import CBOR.Encode
import CBOR.Encoding
import CBOR.Stream
import CBOR.Value
