{- | CBOR (RFC 8949) runtime value representation.

Provides a dynamically-typed Haskell value that can represent any CBOR
data item. Each constructor corresponds to a CBOR major type: unsigned
integers, negative integers, byte strings, text strings, arrays, maps,
tags, and simple values (booleans, null, undefined, floats).

@
import qualified CBOR.Value as C
import qualified CBOR.Encode as CE
import qualified CBOR.Decode as CD
import qualified Data.Vector as V

let val = C.Map (V.fromList [(C.TextString \"key\", C.UInt 42)])
let bytes = CE.encode val
let Right decoded = CD.decode bytes
@
-}
module CBOR.Value (
  Value (..),
) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)


{- | A dynamically-typed CBOR value. Each constructor corresponds to a
CBOR major type or simple value.
-}
data Value
  = UInt {-# UNPACK #-} !Word64
  | -- | negative: represents -1 - n
    NInt {-# UNPACK #-} !Word64
  | Bool !Bool
  | Null
  | Undefined
  | -- | half-precision (stored as Float)
    Float16 {-# UNPACK #-} !Float
  | Float32 {-# UNPACK #-} !Float
  | Float64 {-# UNPACK #-} !Double
  | ByteString !ByteString
  | TextString !Text
  | Array !(Vector Value)
  | Map !(Vector (Value, Value))
  | Tag {-# UNPACK #-} !Word64 !Value
  | -- | Custom simple value. Codes 24 through 31 are reserved.
    Simple {-# UNPACK #-} !Word8
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
