{- | Convenience re-export module for the common wireform-bond surface.

Import this module to get the Bond compact binary codec — encoding,
decoding, the dynamic 'Value' model, and the Template Haskell deriver —
in one go.

@
import Bond
@

Specialized surfaces that are deliberately *not* re-exported here — opt
into them directly when you need them: "Bond.Schema" (the Bond IDL AST),
"Bond.Parser" (the IDL parser), "Bond.CodeGen" (the IDL code generator),
"Bond.Registry" (the type registry), and "Bond.QQ" (the quasiquoter).
-}
module Bond (
  -- * Encoding
  module Bond.Encode,

  -- * Decoding
  module Bond.Decode,

  -- * Dynamic values
  module Bond.Value,

  -- * Deriving
  module Bond.Derive,
) where

import Bond.Decode
import Bond.Derive
import Bond.Encode
import Bond.Value
