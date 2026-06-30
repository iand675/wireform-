{- | Convenience re-export module for the common wireform-bson surface.

Import this module to get the BSON (MongoDB wire format) codec — encoding,
the direct-to-bytes 'Encoding' builder, decoding, the dynamic 'Value' model,
the 'ToBSON'/'FromBSON' typeclasses, and the deriver — in one go.

@
import BSON
@

BSON is a binary-only format with no self-describing JSON bridge, schema
language, code generator, or quasi-quoter; there are no opt-in IDL/codegen
modules for BSON.
-}
module BSON (
  -- * Encoding
  module BSON.Encode,
  module BSON.Encoding,

  -- * Decoding
  module BSON.Decode,

  -- * Dynamic values
  module BSON.Value,

  -- * Typeclass-based codec + deriving
  module BSON.Class,
  module BSON.Derive,
  ) where

import BSON.Class
import BSON.Decode
import BSON.Derive
import BSON.Encode
import BSON.Encoding
import BSON.Value
