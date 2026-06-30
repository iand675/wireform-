{- | Convenience re-export module for the common wireform-yaml surface.

Import this module to get the YAML codec — encoding, decoding, the
dynamic value model, the typeclasses, and the deriver — in one go.

@
import YAML

let bs = encode (TextString "hello")
case decode bs of
  Right val -> pure val
  Left  err -> fail err
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "YAML.JSON" (the YAML ↔ JSON bridge),
"YAML.Annotated" and "YAML.Decode.Annotated" (the position-annotated value
model and decoder), and "YAML.Pretty" (the pretty-printer).
-}
module YAML (
  -- * Encoding
  module YAML.Encode,
  module YAML.Encoding,

  -- * Decoding
  module YAML.Decode,

  -- * Dynamic values
  module YAML.Value,

  -- * Typeclass-based codec + deriving
  module YAML.Class,
  module YAML.Derive,
  ) where

import YAML.Class
import YAML.Decode
import YAML.Derive
import YAML.Encode
import YAML.Encoding
import YAML.Value
