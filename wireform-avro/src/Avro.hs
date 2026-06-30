{- | Convenience re-export module for the common wireform-avro surface.

Import this module to get the Avro codec — encoding, decoding, the
dynamic 'Value' model, the 'ToAvro'/'FromAvro' typeclasses, the deriver,
the direct-to-bytes 'Encoding' builder, the binary 'Wire' primitives, and
the Avro object-container file support — in one go.

@
import Avro
@

Specialized surfaces that are deliberately *not* re-exported here — opt into
them directly when you need them: "Avro.Schema" and "Avro.Schema.Parse" (the
schema model and parser), "Avro.CodeGen" (schema → Haskell codegen),
"Avro.IDL" and "Avro.IDLConvert" (Avro IDL), "Avro.Protocol" (protocol
definitions), "Avro.Registry" (schema registry), "Avro.Resolution" (schema
resolution), "Avro.Fingerprint" (schema fingerprints), "Avro.JSON" (the
Avro ↔ JSON bridge), and "Avro.QQ" (quasiquoter).
-}
module Avro (
  -- * Encoding
  module Avro.Encode,
  module Avro.Encoding,

  -- * Decoding
  module Avro.Decode,

  -- * Binary wire + container files
  module Avro.Wire,
  module Avro.Container,

  -- * Dynamic values
  module Avro.Value,

  -- * Typeclass-based codec + deriving
  module Avro.Class,
  module Avro.Derive,
) where

import Avro.Class
import Avro.Container
import Avro.Decode
import Avro.Derive
import Avro.Encode
import Avro.Encoding
import Avro.Value
import Avro.Wire
