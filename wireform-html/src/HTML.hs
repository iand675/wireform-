{- | Convenience re-export module for the common wireform-html surface.

Import this module to get the HTML codec — encoding, parsing, the
dynamic 'Value' model, the typeclasses, and the deriver — in one go.

@
import HTML
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "HTML.DOM", "HTML.Selector", "HTML.Rewriter",
and "HTML.TagId" (the DOM-manipulation surface).
-}
module HTML (
  -- * Encoding
  module HTML.Encode,
  module HTML.Encoding,

  -- * Decoding
  module HTML.Parse,  -- HTML's decode is HTML.Parse

  -- * Dynamic values
  module HTML.Value,

  -- * Typeclass-based codec + deriving
  module HTML.Class,
  module HTML.Derive,
) where

import HTML.Class
import HTML.Derive
import HTML.Encode
import HTML.Encoding
import HTML.Parse
import HTML.Value
