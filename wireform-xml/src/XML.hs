{- | Convenience re-export module for the common wireform-xml surface.

Import this module to get the XML codec — encoding, decoding, the
dynamic value model, the typeclasses, and the deriver — in one go.

@
import XML
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "XML.Schema" (schema validation / type
generation), "XML.SAX" (streaming SAX parser), "XML.DSL" (builder DSL),
"XML.FastDOM" (the specialized DOM representation), "XML.Generic" (generics-
based encoding/decoding), "XML.Incremental" (incremental decode),
"XML.Path" (XPath), "XML.XSLT" (XSLT transforms), "XML.CodeGen" (codegen),
and "XML.QQ" (quasiquoter).
-}
module XML (
  -- * Encoding
  module XML.Encode,
  module XML.Encoding,

  -- * Decoding
  module XML.Decode,

  -- * Dynamic values
  module XML.Value,

  -- * Typeclass-based codec + deriving
  module XML.Class,
  module XML.Derive,
  ) where

import XML.Class
import XML.Decode
import XML.Derive
import XML.Encode
import XML.Encoding
import XML.Value
