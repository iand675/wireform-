module Proto.Google.Protobuf.Any where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Proto.Decode (MessageDecode, UnknownField)
import Proto.Encode (MessageEncode)

-- The generated Any JSON instances and the standard registry depend on each other.
data Any = Any
  { anyTypeUrl :: !Text
  , anyValue :: !ByteString
  , anyUnknownFields :: ![UnknownField]
  }

instance MessageDecode Any
instance MessageEncode Any
