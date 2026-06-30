{- | Convenience re-export module for the common wireform-fory surface.

Import this module to get the Fory codec — encoding, decoding, the
dynamic 'Value' model, the typeclasses, the deriver, and the struct /
options / type-id vocabulary needed to register and run a codec — in
one go.

@
import Fory
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "Fory.IO" (the IO codec surface),
"Fory.Bulk" (bulk serialization), "Fory.Direct" (the direct-write path),
the meta-string compression modules ("Fory.MetaString",
"Fory.MetaString.Encoder", "Fory.MetaString.Hash"), and "Fory.TextHelpers".
-}
module Fory (
  -- * Encoding
  module Fory.Encode,
  module Fory.Encoding,

  -- * Decoding
  module Fory.Decode,

  -- * Dynamic values
  module Fory.Value,

  -- * Struct schema / options / type ids
  module Fory.Options,
  module Fory.Struct,
  module Fory.TypeId,

  -- * Typeclass-based codec + deriving
  module Fory.Class,
  module Fory.Derive,
) where

import Fory.Class
import Fory.Decode
import Fory.Derive
import Fory.Encode
import Fory.Encoding
import Fory.Options
import Fory.Struct
import Fory.TypeId
import Fory.Value
