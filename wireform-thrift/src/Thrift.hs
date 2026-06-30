{- | Convenience re-export module for the common wireform-thrift surface.

Import this module to get the Thrift binary codec — encoding, decoding, the
wire types, the dynamic 'Value' model, the 'ToThrift'/'FromThrift' typeclasses,
and the deriver — in one go.

@
import Thrift
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "Thrift.Schema" (IDL schema model),
"Thrift.Parser" (IDL parser), "Thrift.CodeGen" (IDL code generation),
"Thrift.Message" (message framing), "Thrift.Transport" (transport layer),
"Thrift.Registry" (type registry), "Thrift.JSON" (Thrift ↔ JSON mapping),
and "Thrift.QQ" (quasiquoter).
-}
module Thrift (
  -- * Encoding
  module Thrift.Encode,
  module Thrift.Encoding,

  -- * Decoding
  module Thrift.Decode,

  -- * Wire format
  module Thrift.Wire,

  -- * Dynamic values
  module Thrift.Value,

  -- * Typeclass-based codec + deriving
  module Thrift.Class,
  module Thrift.Derive,
) where

import Thrift.Class
import Thrift.Decode
import Thrift.Derive
import Thrift.Encode
import Thrift.Encoding
import Thrift.Value
import Thrift.Wire
