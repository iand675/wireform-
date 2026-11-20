{- | EDI segment codecs and annotation-driven deriving. Envelope-specific
helpers live in "EDI.EDIFACT" and "EDI.X12"; standard-family detection
lives in "EDI.Standard".
-}
module EDI (
  module EDI.Value,
  module EDI.Decode,
  module EDI.Encode,
  module EDI.Class,
  module EDI.Derive,
) where

import EDI.Class
import EDI.Decode
import EDI.Derive
import EDI.Encode
import EDI.Value
