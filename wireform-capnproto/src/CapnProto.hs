{- | Convenience re-export module for the common wireform-capnproto surface.

Import this module to get the CapnProto codec — encoding, decoding, the
dynamic value model, and the deriver — in one go.

@
import CapnProto
@

CapnProto does not ship a typeclass-based codec surface (no 'ToCapnProto'/
'FromCapnProto' class module); encoding and decoding are done via the
direct 'encode'/'decode' functions re-exported here.

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "CapnProto.Schema" (the Cap'n Proto schema
model), "CapnProto.Parser" (schema-text parsing), "CapnProto.CodeGen"
(schema-to-Haskell code generation), "CapnProto.Registry" (compiled schema
lookup), and "CapnProto.QQ" (quasiquoter).
-}
module CapnProto (
  -- * Encoding
  module CapnProto.Encode,

  -- * Decoding
  module CapnProto.Decode,

  -- * Dynamic values
  module CapnProto.Value,

  -- * Deriving
  module CapnProto.Derive,
) where

import CapnProto.Decode
import CapnProto.Derive
import CapnProto.Encode
import CapnProto.Value
