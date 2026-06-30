{- | Convenience re-export module for the common wireform-bencode surface.

Import this module to get the Bencode codec — encoding, decoding, the
dynamic value model, the typeclasses, and the deriver — in one go.

@
import Bencode
@

There are no opt-in IDL/codegen modules for Bencode; this module is the
whole public surface.
-}
module Bencode (
  -- * Encoding
  module Bencode.Encode,
  module Bencode.Encoding,

  -- * Decoding
  module Bencode.Decode,

  -- * Dynamic values
  module Bencode.Value,

  -- * Typeclass-based codec + deriving
  module Bencode.Class,
  module Bencode.Derive,
) where

import Bencode.Class
import Bencode.Decode
import Bencode.Derive
import Bencode.Encode
import Bencode.Encoding
import Bencode.Value
