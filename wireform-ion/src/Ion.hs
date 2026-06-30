{- | Convenience re-export module for the common wireform-ion surface.

Import this module to get the Ion codec — encoding, decoding, the
dynamic value model, the typeclasses, and the deriver — in one go.

@
import Ion
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "Ion.SchemaLang", "Ion.ISLSchema",
"Ion.ISLCodeGen" (the Ion Schema Language / code-generation surface),
and "Ion.QQ" (quasiquoter).
-}
module Ion (
  -- * Encoding
  module Ion.Encode,
  module Ion.Encoding,

  -- * Decoding
  module Ion.Decode,

  -- * Dynamic values
  module Ion.Value,

  -- * Typeclass-based codec + deriving
  module Ion.Class,
  module Ion.Derive,
) where

import Ion.Class
import Ion.Decode
import Ion.Derive
import Ion.Encode
import Ion.Encoding
import Ion.Value
