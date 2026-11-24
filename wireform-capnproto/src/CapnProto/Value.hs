{- | Cap'n Proto value representation.

Cap'n Proto is a zero-copy serialization format where data is laid out
in memory exactly as it appears on the wire. This module defines a
dynamically-typed value covering Cap'n Proto's type system: void,
integers, floats, booleans, text, data, structs, lists, enums, and
unions.

@
import qualified CapnProto.Value as CP
import qualified CapnProto.Encode as CPE
import qualified CapnProto.Decode as CPD
import qualified Data.Vector as V

let val = CP.Struct (V.singleton (CP.UInt64 42)) (V.singleton (CP.Text \"hello\"))
let bytes = CPE.encode val
let Right decoded = CPD.decode bytes
@
-}
module CapnProto.Value (
  Value (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)


data Value
  = Void
  | Bool !Bool
  | Int8 {-# UNPACK #-} !Int8
  | Int16 {-# UNPACK #-} !Int16
  | Int32 {-# UNPACK #-} !Int32
  | Int64 {-# UNPACK #-} !Int64
  | UInt8 {-# UNPACK #-} !Word8
  | UInt16 {-# UNPACK #-} !Word16
  | UInt32 {-# UNPACK #-} !Word32
  | UInt64 {-# UNPACK #-} !Word64
  | Float32 {-# UNPACK #-} !Float
  | Float64 {-# UNPACK #-} !Double
  | Text !Text
  | Data !ByteString
  | Struct !(Vector Value) !(Vector Value)
  | List !(Vector Value)
  | Enum {-# UNPACK #-} !Word16
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


instance Aeson.ToJSON Value where
  toJSON Void = Aeson.Null
  toJSON (Bool b) = Aeson.Bool b
  toJSON (Int8 n) = Aeson.Number (fromIntegral n)
  toJSON (Int16 n) = Aeson.Number (fromIntegral n)
  toJSON (Int32 n) = Aeson.Number (fromIntegral n)
  toJSON (Int64 n) = Aeson.Number (fromIntegral n)
  toJSON (UInt8 n) = Aeson.Number (fromIntegral n)
  toJSON (UInt16 n) = Aeson.Number (fromIntegral n)
  toJSON (UInt32 n) = Aeson.Number (fromIntegral n)
  toJSON (UInt64 n) = Aeson.Number (fromIntegral n)
  toJSON (Float32 f) = Aeson.Number (fromFloatDigits f)
  toJSON (Float64 d) = Aeson.Number (fromFloatDigits d)
  toJSON (Text t) = Aeson.String t
  toJSON (Data bs) = Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  toJSON (Struct dataFields ptrFields) =
    Aeson.Object $
      KM.fromList
        [ (Key.fromText "data", Aeson.Array (V.map Aeson.toJSON dataFields))
        , (Key.fromText "pointers", Aeson.Array (V.map Aeson.toJSON ptrFields))
        ]
  toJSON (List vs) = Aeson.Array (V.map Aeson.toJSON vs)
  toJSON (Enum n) = Aeson.Number (fromIntegral n)
