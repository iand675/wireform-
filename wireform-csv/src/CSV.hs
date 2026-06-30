{- | Convenience re-export module for the common wireform-csv surface.

Import this module to get the CSV codec — encoding, decoding, the
dynamic value model, the typeclasses, and the deriver — in one go.

@
import CSV
@

There are no opt-in IDL/codegen modules for CSV; this module re-exports
the entire public codec surface.
-}
module CSV (
  -- * Encoding
  module CSV.Encode,

  -- * Decoding
  module CSV.Decode,

  -- * Dynamic values
  module CSV.Value,

  -- * Typeclass-based codec + deriving
  module CSV.Class,
  module CSV.Derive,
) where

import CSV.Class
import CSV.Decode
import CSV.Derive
import CSV.Encode
import CSV.Value
