-- | FlatBuffers value representation.
--
-- FlatBuffers is Google's zero-copy flat serialization format. This module
-- defines a dynamically-typed value covering FlatBuffers' type system:
-- scalars (integers, floats, booleans), strings, vectors, tables (with
-- optional fields), structs (fixed-size inline), and unions.
--
-- @
-- import qualified FlatBuffers.Value as FB
-- import qualified FlatBuffers.Encode as FBE
-- import qualified FlatBuffers.Decode as FBD
-- import qualified Data.Vector as V
--
-- let val = FB.Table (V.fromList [Just (FB.FBString \"hello\"), Just (FB.Int32 42)])
-- let bytes = FBE.encode val
-- let Right decoded = FBD.decode bytes
-- @
module FlatBuffers.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson

data Value
  = VBool    !Bool
  | VInt8    {-# UNPACK #-} !Int8
  | VInt16   {-# UNPACK #-} !Int16
  | VInt32   {-# UNPACK #-} !Int32
  | VInt64   {-# UNPACK #-} !Int64
  | VWord8   {-# UNPACK #-} !Word8
  | VWord16  {-# UNPACK #-} !Word16
  | VWord32  {-# UNPACK #-} !Word32
  | VWord64  {-# UNPACK #-} !Word64
  | VFloat   {-# UNPACK #-} !Float
  | VDouble  {-# UNPACK #-} !Double
  | VString  !Text
  | VVector  !(Vector Value)
  | VTable   !(Vector (Maybe Value))
  | VStruct  !(Vector Value)
  | VUnion   {-# UNPACK #-} !Word8 !Value
    -- ^ A FlatBuffers union value: the 'Word8' is the union member
    -- index (1-based; @0@ is the spec-defined @NONE@ marker), and the
    -- 'Value' is the referenced table.  Encoded as the standard pair
    -- of vtable cells (a @ubyte@ discriminator and a @uoffset_t@ to
    -- the referenced table).
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON Value where
  toJSON (VBool b)    = Aeson.Bool b
  toJSON (VInt8 n)    = Aeson.Number (fromIntegral n)
  toJSON (VInt16 n)   = Aeson.Number (fromIntegral n)
  toJSON (VInt32 n)   = Aeson.Number (fromIntegral n)
  toJSON (VInt64 n)   = Aeson.Number (fromIntegral n)
  toJSON (VWord8 n)   = Aeson.Number (fromIntegral n)
  toJSON (VWord16 n)  = Aeson.Number (fromIntegral n)
  toJSON (VWord32 n)  = Aeson.Number (fromIntegral n)
  toJSON (VWord64 n)  = Aeson.Number (fromIntegral n)
  toJSON (VFloat f)   = Aeson.Number (fromFloatDigits f)
  toJSON (VDouble d)  = Aeson.Number (fromFloatDigits d)
  toJSON (VString t)  = Aeson.String t
  toJSON (VVector vs) = Aeson.Array (V.map Aeson.toJSON vs)
  toJSON (VTable vs)  = Aeson.Array (V.map (maybe Aeson.Null Aeson.toJSON) vs)
  toJSON (VStruct vs) = Aeson.Array (V.map Aeson.toJSON vs)
  toJSON (VUnion d v) = Aeson.Array (V.fromList [Aeson.Number (fromIntegral d), Aeson.toJSON v])
