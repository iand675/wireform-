{- | Convenience re-export module for the common wireform-toml surface.

Import this module to get the TOML codec — encoding, decoding, the
dynamic value model, the typeclasses, and the deriver — in one go.

@
import TOML
@

There are no opt-in IDL/codegen modules for TOML.
-}
module TOML (
  -- * Encoding
  module TOML.Encode,
  module TOML.Encoding,

  -- * Decoding
  module TOML.Decode,

  -- * Dynamic values
  module TOML.Value,

  -- * Typeclass-based codec + deriving
  module TOML.Class,
  module TOML.Derive,
) where

import TOML.Class
import TOML.Decode
import TOML.Derive
import TOML.Encode
import TOML.Encoding
import TOML.Value
