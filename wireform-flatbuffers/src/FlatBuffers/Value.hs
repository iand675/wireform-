{- | FlatBuffers value representation.

FlatBuffers is Google's zero-copy flat serialization format. This module
defines a dynamically-typed value covering FlatBuffers' type system:
scalars (integers, floats, booleans), strings, vectors, tables (with
optional fields), structs (fixed-size inline), and unions.

The dynamic codec requires a schema: use @encode schema value@ and
@decode schema bytes@. Table and struct vectors follow declaration order.
A @VUnion tag child@ occupies one logical table field; tag 1 denotes the
first member in the schema. An absent union is represented by Nothing.
-}
module FlatBuffers.Value (
  Value (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson qualified as Aeson
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)


data Value
  = VBool !Bool
  | VInt8 {-# UNPACK #-} !Int8
  | VInt16 {-# UNPACK #-} !Int16
  | VInt32 {-# UNPACK #-} !Int32
  | VInt64 {-# UNPACK #-} !Int64
  | VWord8 {-# UNPACK #-} !Word8
  | VWord16 {-# UNPACK #-} !Word16
  | VWord32 {-# UNPACK #-} !Word32
  | VWord64 {-# UNPACK #-} !Word64
  | VFloat {-# UNPACK #-} !Float
  | VDouble {-# UNPACK #-} !Double
  | VString !Text
  | VVector !(Vector Value)
  | VTable !(Vector (Maybe Value))
  | VStruct !(Vector Value)
  | VUnion {-# UNPACK #-} !Word8 !Value
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


instance Aeson.ToJSON Value where
  toJSON (VBool b) = Aeson.Bool b
  toJSON (VInt8 n) = Aeson.Number (fromIntegral n)
  toJSON (VInt16 n) = Aeson.Number (fromIntegral n)
  toJSON (VInt32 n) = Aeson.Number (fromIntegral n)
  toJSON (VInt64 n) = Aeson.Number (fromIntegral n)
  toJSON (VWord8 n) = Aeson.Number (fromIntegral n)
  toJSON (VWord16 n) = Aeson.Number (fromIntegral n)
  toJSON (VWord32 n) = Aeson.Number (fromIntegral n)
  toJSON (VWord64 n) = Aeson.Number (fromIntegral n)
  toJSON (VFloat f) = Aeson.Number (fromFloatDigits f)
  toJSON (VDouble d) = Aeson.Number (fromFloatDigits d)
  toJSON (VString t) = Aeson.String t
  toJSON (VVector vs) = Aeson.Array (V.map Aeson.toJSON vs)
  toJSON (VTable vs) = Aeson.Array (V.map (maybe Aeson.Null Aeson.toJSON) vs)
  toJSON (VStruct vs) = Aeson.Array (V.map Aeson.toJSON vs)
  toJSON (VUnion tag value) = Aeson.object ["tag" Aeson..= tag, "value" Aeson..= value]
