{- | Convenience re-export module for the common wireform-msgpack surface.

Import this module to get the MsgPack codec — encoding, decoding, the
direct-to-bytes 'Encoding' builder, streaming decode, the dynamic
'Value' model, the 'ToMsgPack'/'FromMsgPack' typeclasses, and the
deriver — in one go.

@
import MsgPack

let bs  = encode (MsgString "hello")
case decode bs of
  Right val -> pure val
  Left  err -> fail err
@

Specialized surfaces that are deliberately *not* re-exported here — opt
into them directly when you need them: "MsgPack.JSON" (the
self-describing MsgPack ↔ JSON bridge) and "MsgPack.RPC" (the RPC layer).
-}
module MsgPack (
  -- * Encoding
  module MsgPack.Encode,
  module MsgPack.Encoding,

  -- * Decoding
  module MsgPack.Decode,
  module MsgPack.Stream,

  -- * Dynamic values
  module MsgPack.Value,

  -- * Typeclass-based codec + deriving
  module MsgPack.Class,
  module MsgPack.Derive,
  ) where

import MsgPack.Class
import MsgPack.Decode
import MsgPack.Derive
import MsgPack.Encode
import MsgPack.Encoding
import MsgPack.Stream
import MsgPack.Value
