{- | Convenience re-export module for the common wireform-ndjson surface.

NDJSON is line-framed JSON on top of aeson. Import this module to get the
NDJSON codec — encoding, decoding, and the deriver — in one go.

@
import NDJSON
@

There are no specialized IDL/codegen/QQ/JSON-bridge surfaces for NDJSON; every
public module is re-exported here.
-}
module NDJSON (
  -- * Encoding
  module NDJSON.Encode,

  -- * Decoding
  module NDJSON.Decode,

  -- * Deriving
  module NDJSON.Derive,
) where

import NDJSON.Decode
import NDJSON.Derive
import NDJSON.Encode
