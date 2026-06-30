{- | Convenience re-export module for the common wireform-flatbuffers surface.

Import this module to get the FlatBuffers codec — encoding, decoding, the
dynamic value model, and the deriver — in one go.

@
import FlatBuffers
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "FlatBuffers.Builder", "FlatBuffers.Reader",
"FlatBuffers.View", "FlatBuffers.Schema", "FlatBuffers.Parser",
"FlatBuffers.CodeGen", "FlatBuffers.Registry", and "FlatBuffers.QQ".
-}
module FlatBuffers (
  -- * Encoding
  module FlatBuffers.Encode,

  -- * Decoding
  module FlatBuffers.Decode,

  -- * Dynamic values
  module FlatBuffers.Value,

  -- * Deriving
  module FlatBuffers.Derive,
) where

import FlatBuffers.Decode
import FlatBuffers.Derive
import FlatBuffers.Encode
import FlatBuffers.Value
