{- | Convenience re-export module for the common wireform-edn surface.

Import this module to get the EDN codec — encoding, decoding, the
dynamic 'Value' model, the typeclasses, and the deriver — in one go.

@
import EDN
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "EDN.JSON" (the EDN ↔ JSON bridge).
-}
module EDN (
  -- * Encoding
  module EDN.Encode,
  module EDN.Encoding,

  -- * Decoding
  module EDN.Decode,

  -- * Dynamic values
  module EDN.Value,

  -- * Typeclass-based codec + deriving
  module EDN.Class,
  module EDN.Derive,
) where

import EDN.Class
import EDN.Decode
import EDN.Derive
import EDN.Encode
import EDN.Encoding
import EDN.Value
